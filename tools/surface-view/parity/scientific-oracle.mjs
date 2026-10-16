/** Independent source-geometry ray oracle: no Three.js or ScalaFIM matrices,
 * packets, rendered buffers or picks enter the calculation. */
const add = (a, b) => a.map((x, i) => x + b[i]);
const sub = (a, b) => a.map((x, i) => x - b[i]);
const mul = (a, s) => a.map(x => x * s);
const dot = (a, b) => a.reduce((s, x, i) => s + x * b[i], 0);
const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
const unit = a => mul(a, 1 / Math.sqrt(dot(a, a)));

/** Closed-form field oracle for the frozen analytic volume. The inverse is
 * solved from source affine columns, independently of both image providers. */
export function analyticVolumeSamples(spec) {
  return spec.surfaces.flatMap(surface => (surface.layers ?? []).filter(layer => layer.kind === 'volume').map(layer => {
    if (layer.values !== 'i + 2*j + 3*k + 10*t' || layer.matrixOrder !== 'row-major' || !['nearest', 'linear'].includes(layer.interpolation)) throw new Error('unsupported analytic volume oracle');
    const m = layer.voxelToWorld, columns = [[m[0], m[4], m[8]], [m[1], m[5], m[9]], [m[2], m[6], m[10]]];
    const determinant = dot(columns[0], cross(columns[1], columns[2]));
    if (!Number.isFinite(determinant) || Math.abs(determinant) < 1e-12) throw new Error('singular analytic affine');
    const values = surface.vertices.map(vertex => {
      const world = vertex.map((x, i) => x + (surface.placement?.[i] ?? 0) - m[i * 4 + 3]);
      const ijk = [dot(world, cross(columns[1], columns[2])), dot(columns[0], cross(world, columns[2])), dot(columns[0], cross(columns[1], world))].map(x => x / determinant);
      const sample = layer.interpolation === 'nearest' ? ijk.map(x => Math.floor(x + .5)) : ijk;
      if (sample.some((x, i) => x < 0 || x > layer.dims[i] - 1)) return null;
      return sample[0] + 2 * sample[1] + 3 * sample[2] + 10 * layer.frame;
    });
    return { surface: surface.id, layer: layer.id, values };
  }));
}

export function compareVolumeSamples(expected, actual) {
  if (!Array.isArray(actual) || expected.length !== actual.length) return { pass: false, expected, actual };
  const checks = expected.map(want => {
    const matches = actual.filter(got => got.surface === want.surface && got.layer === want.layer);
    const got = matches[0];
    return matches.length === 1 && Array.isArray(got.values) && got.values.length === want.values.length &&
      want.values.every((value, i) => value === null ? got.values[i] === null : Number.isFinite(got.values[i]) && Math.abs(value - got.values[i]) <= 1e-9);
  });
  return { pass: checks.every(Boolean), checks, expected, actual };
}

export function scientificPick(spec, x, y, pixelCentre = false) {
  if (spec.views) {
    if (pixelCentre) { x = Math.floor(x) + .5; y = Math.floor(y) + .5; }
    const view = spec.views.find(v => x >= v.viewport[0] * spec.width && x < (v.viewport[0] + v.viewport[2]) * spec.width &&
      y >= v.viewport[1] * spec.height && y < (v.viewport[1] + v.viewport[3]) * spec.height);
    if (!view) return null;
    const { views: omitted, ...single } = spec;
    const hit = scientificPick({ ...single, camera: view.camera, surfaces: spec.surfaces.filter(s => view.surfaces.includes(s.id)),
      width: view.viewport[2] * spec.width, height: view.viewport[3] * spec.height },
      x - view.viewport[0] * spec.width, y - view.viewport[1] * spec.height);
    return hit && { ...hit, view: view.id };
  }
  if (spec.surfaces.some(s => s.annotations?.length || s.crosshair || s.layers?.some(l => !['scalar', 'rgba', 'temporal', 'labels', 'volume'].includes(l.kind)))) throw new Error('scientific-only oracle cannot omit decorations');
  const c = spec.camera;
  if (pixelCentre) { x = Math.floor(x) + .5; y = Math.floor(y) + .5; }
  if (x < 0 || y < 0 || x >= spec.width || y >= spec.height) return null;
  const forward = unit(sub(c.target, c.eye)), right = unit(cross(forward, c.up)), up = cross(right, forward);
  const nx = x / spec.width * 2 - 1, ny = 1 - y / spec.height * 2;
  let origin = c.eye, direction = forward;
  if (c.projection === 'perspective') {
    const tangent = Math.tan(c.fovDegrees * Math.PI / 360);
    direction = unit(add(forward, add(mul(right, nx * tangent * c.aspect), mul(up, ny * tangent))));
  } else if (c.projection === 'orthographic') {
    origin = add(origin, add(mul(right, c.left + (nx + 1) / 2 * (c.right - c.left)), mul(up, c.bottom + (ny + 1) / 2 * (c.top - c.bottom))));
  } else throw new Error('unsupported oracle projection');
  let winner = null;
  for (const s of spec.surfaces) {
    if (s.alpha === 0) continue;
    const vertices = s.vertices.map((p, i) => p.map((v, j) => v + (s.morph ? s.morph.weight * (s.morph.vertices[i][j] - v) : 0) + (s.placement?.[j] ?? 0)));
    for (let face = 0; face < s.faces.length; face++) {
      const ids = s.faces[face], [a, b, d] = ids.map(i => vertices[i]);
      const e1 = sub(b, a), e2 = sub(d, a), p = cross(direction, e2), determinant = dot(e1, p);
      if (Math.abs(determinant) < 1e-13) continue;
      const tvec = sub(origin, a), wb = dot(tvec, p) / determinant, q = cross(tvec, e1), wc = dot(direction, q) / determinant;
      if (wb < -1e-10 || wc < -1e-10 || wb + wc > 1 + 1e-10) continue;
      const distance = dot(e2, q) / determinant;
      if (distance < 0 || (winner && distance >= winner.distance - 1e-10)) continue;
      const world = add(origin, mul(direction, distance)), depth = dot(sub(world, c.eye), forward);
      if (depth < c.near || depth > c.far) continue;
      if (s.clip && dot(s.clip.normal, sub(world, s.clip.point)) * (s.clip.flip ? -1 : 1) < 0) continue;
      const weights = [1 - wb - wc, wb, wc], maximum = Math.max(...weights);
      const vertex = Math.min(...ids.filter((_, i) => Math.abs(weights[i] - maximum) < 1e-12));
      const closest = ids.reduce((best, id) => dot(sub(vertices[id], world), sub(vertices[id], world)) < dot(sub(vertices[best], world), sub(vertices[best], world)) ? id : best, ids[0]);
      winner = { surface: s.id, face, vertex, euclideanVertex: closest, world, weights, distance };
    }
  }
  return winner;
}

export function compareScientificPick(expected, actual, vertexPolicy = 'barycentric') {
  if (!expected || !actual) return { pass: !expected && !actual, expected, actual };
  const vertex = vertexPolicy === 'euclidean' ? expected.euclideanVertex : expected.vertex;
  const worldError = actual.world ? Math.max(...actual.world.map((v, i) => Math.abs(v - expected.world[i]))) : null;
  return { pass: actual.surface === expected.surface && actual.vertex === vertex && (expected.view === undefined || actual.view === expected.view) && (actual.face === undefined || actual.face === expected.face) && (worldError === null || worldError <= 1e-3),
    expected: { surface: expected.surface, face: expected.face, vertex, world: expected.world }, actual, worldError };
}

/** Source vertex interpolation only; independent of either renderer's geometry. */
export function annotationCenters(spec) {
  return spec.surfaces.flatMap(s => (s.annotations ?? []).map(a => ({id:a.id, surface:s.id, vertex:a.vertex,
    world:s.vertices[a.vertex].map((v,axis) => v + (s.morph ? s.morph.weight*(s.morph.vertices[a.vertex][axis]-v) : 0) + (s.placement?.[axis] ?? 0))})));
}
export function compareAnnotationCenters(expected, actual) {
  if (!Array.isArray(actual) || expected.length !== actual.length) return {pass:false, expected, actual};
  const checks=expected.map(want => {
    const matches=actual.filter(got=>got.id===want.id && got.surface===want.surface && got.vertex===want.vertex);
    const got=matches[0];
    const error=matches.length===1 && Array.isArray(got.world) && got.world.length===3 && got.world.every(Number.isFinite)
      ? Math.max(...got.world.map((v,i)=>Math.abs(v-want.world[i]))) : null;
    return {pass:error!==null && error<=1e-4, maximumWorldError:error, expected:want, actual:got};
  });
  return {pass:checks.every(x=>x.pass),checks};
}

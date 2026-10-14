/** Frozen scientific inputs, not renderer output or parity evidence. */
const finite3 = x => Array.isArray(x) && x.length === 3 && x.every(Number.isFinite);
const mesh = ({ vertices, faces }) => { if (!Array.isArray(vertices) || vertices.length === 0 || !vertices.every(finite3) || !Array.isArray(faces) || !faces.every(f => Array.isArray(f) && f.length === 3 && f.every(i => Number.isInteger(i) && i >= 0 && i < vertices.length)))
    throw new TypeError('invalid mesh'); };
export const tetra = { vertices: [[0, 0, 0], [1.4, .1, .2], [-.2, 1.1, .35], [.15, -.35, 1.3]], faces: [[0, 1, 2], [0, 3, 1], [0, 2, 3], [1, 3, 2]] };
export const square2tri = { vertices: [[-1, -1, 0], [1, -1, 0], [-1, 1, 0], [1, 1, 0]], faces: [[0, 1, 2], [1, 3, 2]] };
export const externalCortex = { kind: 'external-fsaverage5-pial', binaryWrite: false, left: { file: 'fsaverage5-lh-pial.gii', sha256: 'b425b4914362af4aaf43bb5d022afd39a0c5f6ec4601e353631cdd737884c951' }, right: { file: 'fsaverage5-rh-pial.gii', sha256: '6142dfcec7533aaeb962b1de829e21948bb35c85645a08f2c4b7ea503d655ee9' } };
export const viewports = { square: [512, 512], wide: [600, 450], tall: [450, 600] };
export const exportSizes = { manuscript: [2400, 1800], screen: [1200, 900] };
export const galleryControls = { opacity: [.25, .5, .85, 1], planes: { axisX: { normal: [1, 0, 0], offset: 0 }, axisY: { normal: [0, 1, 0], offset: 0 }, oblique: { normal: [.577350269, .577350269, .577350269], offset: .15 } }, flatMesh: square2tri, polygonRoi: [[-1, -1], [.1, -1], [.1, .1], [-1, .1]], boundaryInclusion: 'inside-or-edge' };
export const referenceProfiles = {
    observedDefault: { name: 'observed-default', observed: true, qualification: 'pending' },
    controlledUnlit: { name: 'controlled-unlit', ambientIntensity: Math.PI, directionalIntensity: 0,
        direction: [0, 0, 1], fillIntensity: 0, specular: 0, emissive: 0, emissiveIntensity: 0, base: 204, lightFrame: 'world', qualification: 'pending' },
    controlledLit: { name: 'controlled-lit', ambientIntensity: .35 * Math.PI, directionalIntensity: .65 * Math.PI,
        direction: [-.4, -.5, .7681145747868608], fillIntensity: 0, specular: 0, emissive: 0, emissiveIntensity: 0, base: 204, lightFrame: 'world', qualification: 'pending' }
};
const cortical = { leftLateral: [[-350, -20, 20], [-30, -20, 20], [0, 0, 1]], leftMedial: [[290, -20, 20], [-30, -20, 20], [0, 0, 1]], rightLateral: [[350, -20, 20], [30, -20, 20], [0, 0, 1]], rightMedial: [[-290, -20, 20], [30, -20, 20], [0, 0, 1]], dorsal: [[0, -20, 350], [0, -20, 20], [0, 1, 0]], ventral: [[0, -20, -310], [0, -20, 20], [0, 1, 0]] };
/** Exact camera inputs; `kind` is cortical view name or `analytic-dorsal`. */
export function cameraFor(kind, viewName = 'wide', size = viewports[viewName], projection = 'perspective') {
    if (!Array.isArray(size) || size.length !== 2 || !size.every(x => Number.isFinite(x) && x > 0) || !['perspective', 'orthographic'].includes(projection))
        throw new TypeError('invalid camera request');
    const entry = kind === 'analytic-dorsal' ? [[.6, .375, 4.65], [.6, .375, .65], [0, 1, 0]] : cortical[kind];
    if (!entry)
        throw new TypeError(`unknown camera '${kind}'`);
    const [eye, target, up] = entry;
    const common = { eye: [...eye], target: [...target], up: [...up], projection, near: .1, far: 2000, aspect: size[0] / size[1] };
    const halfHeight = kind === 'analytic-dorsal' ? 1.5 : 110;
    const halfWidth = halfHeight * common.aspect;
    return projection === 'perspective' ? { ...common, fovDegrees: 35 }
        : { ...common, left: -halfWidth, right: halfWidth, bottom: -halfHeight, top: halfHeight };
}
export const corticalCameras = Object.fromEntries(Object.keys(cortical).map(k => [k, cameraFor(k)]));
export function spatialFields({ vertices, hemisphere }) {
    mesh({ vertices, faces: [] });
    if (!['left', 'right'].includes(hemisphere))
        throw new TypeError('hemisphere');
    const phase = hemisphere === 'left' ? 0 : Math.PI / 3;
    const value = ([x, y, z], frame) => Math.max(-3, Math.min(3, Math.sin(x / 17 + phase + frame * .31) + Math.cos(y / 23) + Math.sin(z / 29)));
    return { scalar: vertices.map(p => value(p, 0)), time: [0, 1, 2].map(frame => vertices.map(p => value(p, frame))), range: [-3, 3] };
}
export function nearestAnchors(vertices, landmarks) { if (!Array.isArray(vertices) || vertices.length === 0 || !vertices.every(finite3) || !Array.isArray(landmarks) || landmarks.length !== 4 || !landmarks.every(finite3))
    throw new TypeError('four landmarks'); return landmarks.map(mark => vertices.reduce((best, p, index) => { const d = p.reduce((s, v, a) => s + (v - mark[a]) ** 2, 0); return d < best.d ? { index, d } : best; }, { index: -1, d: Infinity }).index); }
export function coreCortex({ vertices, faces, hemisphere }) {
    mesh({ vertices, faces });
    if (faces.length === 0)
        throw new TypeError('cortex requires faces');
    const fields = spatialFields({ vertices, hemisphere });
    const anchors = hemisphere === 'left' ? [[-40, -20, 20], [-30, 20, 35], [-55, -35, 20], [-15, -10, 55]] : [[40, -20, 20], [30, 20, 35], [55, -35, 20], [15, -10, 55]];
    const origin = hemisphere === 'left' ? [-30, -20, 20] : [30, -20, 20];
    const curvature = vertices.map(([x, y, z]) => Math.max(-1, Math.min(1, .5 * Math.sin(x / 19) + .3 * Math.cos(y / 31) - .2 * Math.sin(z / 37))));
    return {
        vertices, faces, hemisphere, fields, curvature,
        labels: vertices.map(([x, y, z]) => Math.abs(x) < 8 ? 0 : 1 + (y >= -20 ? 2 : 0) + (z >= 20 ? 1 : 0)),
        medialWallLabel: 0, anchorLandmarks: anchors, anchors: nearestAnchors(vertices, anchors),
        networkEdges: [[0, 1, .8], [1, 2, -.6], [2, 3, .4], [3, 0, -.2]],
        geometryVariant: { kind: 'synthetic-expansion', scientific: false, vertices: vertices.map(p => p.map((v, a) => origin[a] + 1.2 * (v - origin[a]))), faces, description: 'topology-preserving synthetic expansion, not a scientific inflated brain' }
    };
}
export function linearVolume(x, y, z, t = 0) { return [x, y, z, t].every(Number.isFinite) ? x + 2 * y + 3 * z + 10 * t : NaN; }
export function crossLabelMeshEdges(faces, labels) { const out = new Map(); for (const f of faces)
    for (const [a, b] of [[f[0], f[1]], [f[1], f[2]], [f[2], f[0]]])
        if (labels[a] !== labels[b])
            out.set(`${Math.min(a, b)}:${Math.max(a, b)}`, [Math.min(a, b), Math.max(a, b)]); return [...out.values()]; }
export function polygonVertexSelection(vertices, polygon) { if (!Array.isArray(vertices) || !vertices.every(finite3) || !Array.isArray(polygon) || polygon.length < 3)
    throw new TypeError('polygon'); const on = (x, y, a, b) => Math.abs((b[0] - a[0]) * (y - a[1]) - (b[1] - a[1]) * (x - a[0])) < 1e-12 && x >= Math.min(a[0], b[0]) && x <= Math.max(a[0], b[0]) && y >= Math.min(a[1], b[1]) && y <= Math.max(a[1], b[1]); const inside = ([x, y]) => { if (polygon.some((a, i) => on(x, y, a, polygon[(i + 1) % polygon.length])))
    return true; let hit = false; for (let i = 0, j = polygon.length - 1; i < polygon.length; j = i++)
    if ((polygon[i][1] > y) !== (polygon[j][1] > y) && x < (polygon[j][0] - polygon[i][0]) * (y - polygon[i][1]) / (polygon[j][1] - polygon[i][1]) + polygon[i][0])
        hit = !hit; return hit; }; return vertices.map((p, i) => inside(p) ? i : null).filter(Number.isInteger); }
export function scalarLut(value, { min = -1, max = 1, colors = [[0, 0, 255, 255], [255, 0, 0, 255]], threshold } = {}) {
    if (!Number.isFinite(value))
        return [0, 0, 0, 0];
    if (!(max > min) || !Array.isArray(colors) || colors.length < 2 || !colors.every(c => Array.isArray(c) && c.length === 4 && c.every(Number.isFinite)))
        throw new TypeError('LUT');
    if (threshold && (!Array.isArray(threshold) || threshold.length !== 2 || !threshold.every(Number.isFinite)))
        throw new TypeError('threshold');
    // DataLayer delegates to ColorMap: a zero-width range disables masking.
    const hidden = threshold && threshold[0] !== threshold[1] && value >= Math.min(...threshold) && value <= Math.max(...threshold);
    if (hidden)
        return [0, 0, 0, 0];
    return colors[Math.floor(Math.max(0, Math.min(1, (value - min) / (max - min))) * (colors.length - 1))];
}
export function straightAlpha(over, under) { if (!Array.isArray(over) || !Array.isArray(under) || over.length !== 4 || under.length !== 4)
    throw new TypeError('RGBA'); const a = over[3] + under[3] * (1 - over[3]); return a === 0 ? [0, 0, 0, 0] : [0, 1, 2].map(i => (over[i] * over[3] + under[i] * under[3] * (1 - over[3])) / a).concat(a); }
const lin = x => { x /= 255; return x <= .04045 ? x / 12.92 : ((x + .055) / 1.055) ** 2.4; };
const srgb = x => 255 * (x <= .0031308 ? 12.92 * x : 1.055 * x ** (1 / 2.4) - .055);
export function srgbLinearInterpolate(a, b, t) { if (!Array.isArray(a) || !Array.isArray(b) || a.length !== b.length || !Number.isFinite(t))
    throw new TypeError('colour'); return a.map((x, i) => Math.round(srgb(lin(x) * (1 - t) + lin(b[i]) * t))); }
export function landmarkProjection(point, c) {
    if (!finite3(point) || !finite3(c?.eye) || !finite3(c?.target) || !finite3(c?.up) || !(c.aspect > 0))
        throw new TypeError('projection');
    const sub = (a, b) => a.map((v, i) => v - b[i]), dot = (a, b) => a.reduce((s, v, i) => s + v * b[i], 0), unit = a => a.map(v => v / Math.sqrt(dot(a, a))), cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
    const forward = unit(sub(c.target, c.eye)), right = unit(cross(forward, c.up)), up = cross(right, forward), delta = sub(point, c.eye);
    if (c.projection === 'orthographic') {
        if (![c.left, c.right, c.bottom, c.top].every(Number.isFinite) || !(c.right > c.left) || !(c.top > c.bottom))
            throw new TypeError('orthographic bounds');
        return [(dot(delta, right) - c.left) / (c.right - c.left), 1 - (dot(delta, up) - c.bottom) / (c.top - c.bottom)];
    }
    if (!(c.fovDegrees > 0))
        throw new TypeError('perspective fov');
    const scale = Math.tan(c.fovDegrees * Math.PI / 360) * dot(delta, forward);
    return [.5 + dot(delta, right) / (2 * scale * c.aspect), .5 - dot(delta, up) / (2 * scale)];
}
const common = { viewports, exportSizes, galleryControls, referenceProfiles };
const cortex = { ...common, input: externalCortex, cameras: corticalCameras }, analytic = { ...common, input: tetra, cameras: { dorsal: cameraFor('analytic-dorsal') } }, square = { ...common, input: square2tri, cameras: { dorsal: cameraFor('analytic-dorsal') } };
export const fixtureSpecs = { 'fsaverage5-bilateral-pial-curvature.v1': cortex, 'analytic-asymmetric-scalar-boundaries.v1': square, 'analytic-volume-to-surface-sampling.v1': { ...square, volume: 'x+2y+3z+10t' }, 'analytic-asymmetric-curvature-two-maps.v1': analytic, 'fsaverage5-bilateral-instances.v1': cortex, 'fsaverage5-label-roi-boundaries.v1': cortex, 'fsaverage5-markers-crosshair.v1': cortex, 'analytic-overlap-clipping-alpha.v1': square, 'fsaverage5-time-morph.v1': cortex, 'fsaverage5-signed-network.v1': cortex, 'analytic-mapping-key.v1': square, 'flatmap-linked-selection.v1': { ...square, flatMesh: square2tri, polygonRoi: galleryControls.polygonRoi }, 'fsaverage5-publication-state.v1': cortex };
export function fixtureSpec(id) { if (typeof id !== 'string' || !fixtureSpecs[id])
    throw new TypeError(`unknown fixture '${id}'`); return fixtureSpecs[id]; }

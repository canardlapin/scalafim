/** Source-input preparation shared by reference and candidate adapters.
 * No renderer/compiler output enters this module. External cortex stays local.
 */
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createHash } from 'node:crypto';
import { unzipSync } from 'node:zlib';
import { externalCortex, coreCortex, tetra, square2tri, cameraFor, referenceProfiles, landmarkProjection } from './fixtures.mjs';
import { palette } from './gallery.mjs';

export function decodeGifti(bytes, expectedHash) {
  if (createHash('sha256').update(bytes).digest('hex') !== expectedHash) throw new Error('external GIFTI checksum mismatch');
  const arrays = [...bytes.toString('utf8').matchAll(/<DataArray\b([^>]*)>([\s\S]*?)<\/DataArray>/g)];
  const parsed = new Map();
  for (const [, attributes, body] of arrays) {
    const attrs = Object.fromEntries([...attributes.matchAll(/(\w+)="([^"]*)"/g)].map(x => [x[1], x[2]]));
    if (!['NIFTI_INTENT_POINTSET', 'NIFTI_INTENT_TRIANGLE'].includes(attrs.Intent)) continue;
    if (attrs.Endian !== 'LittleEndian' || attrs.ArrayIndexingOrder !== 'RowMajorOrder' || attrs.Encoding !== 'GZipBase64Binary' || attrs.Dimensionality !== '2' || attrs.Dim1 !== '3') throw new Error('unsupported GIFTI encoding');
    const expectedType = attrs.Intent === 'NIFTI_INTENT_POINTSET' ? 'NIFTI_TYPE_FLOAT32' : 'NIFTI_TYPE_INT32';
    if (attrs.DataType !== expectedType) throw new Error('unsupported GIFTI data type');
    const transform = body.match(/<MatrixData>([\s\S]*?)<\/MatrixData>/)?.[1].trim().split(/\s+/).map(Number);
    if (transform && transform.some((v, i) => v !== (i % 5 === 0 ? 1 : 0))) throw new Error('nonidentity GIFTI transform requires an explicit frame adapter');
    const raw = unzipSync(Buffer.from(body.match(/<Data>([\s\S]*?)<\/Data>/)?.[1] ?? '', 'base64'));
    const count = Number(attrs.Dim0);
    if (!Number.isInteger(count) || count <= 0 || raw.length !== count * 12) throw new Error('GIFTI size mismatch');
    if (parsed.has(attrs.Intent)) throw new Error('duplicate GIFTI intent');
    parsed.set(attrs.Intent, Array.from({ length: count }, (_, i) => Array.from({ length: 3 }, (_, j) => attrs.Intent === 'NIFTI_INTENT_POINTSET' ? raw.readFloatLE(i * 12 + j * 4) : raw.readInt32LE(i * 12 + j * 4))));
  }
  const vertices = parsed.get('NIFTI_INTENT_POINTSET'), faces = parsed.get('NIFTI_INTENT_TRIANGLE');
  if (!vertices?.length || !faces?.length || vertices.some(p => p.some(x => !Number.isFinite(x))) || faces.some(f => f.some(i => i < 0 || i >= vertices.length))) throw new Error('invalid GIFTI mesh');
  return { vertices, faces };
}

export async function loadCortex(directory) {
  return Object.fromEntries(await Promise.all(['left', 'right'].map(async hemisphere => {
    const asset = externalCortex[hemisphere];
    return [hemisphere, coreCortex({ ...decodeGifti(await readFile(resolve(directory, asset.file)), asset.sha256), hemisphere })];
  })));
}

// Rotate in world RAS axes, not by altering geometry or fitting the mesh.
export function cameraAction(original, action = {}) {
  const camera = structuredClone(original);
  const target = camera.target, offset = camera.eye.map((v, i) => v - target[i]);
  const yaw = (action.azimuthDegrees ?? 0) * Math.PI / 180, pitch = (action.elevationDegrees ?? 0) * Math.PI / 180;
  const yawed = [offset[0] * Math.cos(yaw) - offset[1] * Math.sin(yaw), offset[0] * Math.sin(yaw) + offset[1] * Math.cos(yaw), offset[2]];
  const horizontal = Math.hypot(yawed[0], yawed[1]);
  const axis = horizontal > 1e-12 ? [-yawed[1] / horizontal, yawed[0] / horizontal, 0] : [1, 0, 0];
  const cross = [axis[1] * yawed[2], -axis[0] * yawed[2], axis[0] * yawed[1] - axis[1] * yawed[0]];
  const rotated = yawed.map((v, i) => (v * Math.cos(pitch) + cross[i] * Math.sin(pitch)) * (action.eyeDistanceScale ?? 1));
  camera.target = target.map((v, i) => v + (action.worldTranslation?.[i] ?? 0));
  camera.eye = rotated.map((v, i) => v + camera.target[i]);
  return camera;
}

const scalar = (id, values, mapping, extra = {}) => ({ id, kind: 'scalar', values: values.map(x => x === 'NaN' ? 'NaN' : Math.fround(x)), ...mapping, thresholdEdges: true, ...extra });
const corticalSurface = (cortex, hemisphere) => {
  const source = cortex[hemisphere];
  if (!source) throw new Error(`missing external ${hemisphere} cortex`);
  return { id: hemisphere, hemisphere, vertices: source.vertices, faces: source.faces, curvature: source.curvature.map(Math.fround), showCurvature: true, layers: [] };
};

/** Materialize the declared source volume, x fastest. This is source data,
 * not projection: reference and candidate still sample it independently. */
export function volumeFrame(volume) {
  const { dims, frame, values } = volume;
  if (values !== 'i + 2*j + 3*k + 10*t' || dims.length !== 4 || dims.some(x => !Number.isInteger(x) || x <= 0) || !Number.isInteger(frame) || frame < 0 || frame >= dims[3]) throw new Error('unsupported analytic source volume');
  const [nx, ny, nz] = dims;
  return Array.from({ length: nx * ny * nz }, (_, index) => {
    const i = index % nx, j = Math.floor(index / nx) % ny, k = Math.floor(index / (nx * ny));
    return i + 2 * j + 3 * k + 10 * frame;
  });
}

export function qualificationSpec(variant, cortex) {
  const c = variant.effectiveInput, p = c.parameters, family = c.rowId.slice(0, 3);
  const size = p.action?.size ?? c.viewport.size;
  let camera = c.bundle?.view ? cameraFor(c.bundle.view, 'explicit', size, c.bundle.projection) : { ...structuredClone(c.camera), aspect: size[0] / size[1] };
  camera = cameraAction(camera, p.action);
  if (c.bundle?.zoom) camera = cameraAction(camera, { eyeDistanceScale: 1 / c.bundle.zoom });
  const profile = c.bundle?.profile ? referenceProfiles[c.bundle.profile] : c.profile;
  if (!profile) throw new Error('unknown material profile');
  const spec = { width: size[0], height: size[1], camera, profile, surfaces: [], timeSeconds: p.timeSeconds ?? 0 };
  const one = () => corticalSurface(cortex, 'left');
  switch (family) {
    case 'C01':
      spec.surfaces = ['left', 'right'].map(h => corticalSurface(cortex, h));
      break;
    case 'C02': {
      const surface = { id: 'analytic', hemisphere: 'left', ...structuredClone(square2tri), layers: [] };
      surface.layers = p.volume ? [{ id: 'sampled-volume', kind: 'volume', ...p.volume, voxelValues: volumeFrame(p.volume) }]
        : [scalar('scalar', p.values, p.mapping, { thresholdEdges: c.checkpoint === 'sharp-edge' })];
      spec.surfaces = [surface];
      break;
    }
    case 'C03': {
      const layers = { 'scalar-a': scalar('scalar-a', p.scalarA.values, { ...p.scalarA, palette: p.palette }),
        'scalar-b': scalar('scalar-b', p.valuesB ?? p.scalarB.values, { ...p.scalarB, palette: p.palette }, { opacity: p.opacityB ?? p.scalarB.opacity, visible: p.visibleB ?? true, blendMode: p.blendModeB }) };
      spec.surfaces = [{ id: 'analytic', hemisphere: 'left', ...structuredClone(tetra), curvature: [-.8, .25, .7, -.15], showCurvature: true,
        layers: p.layerOrder.filter(id => id !== 'curvature').map(id => layers[id]) }];
      break;
    }
    case 'C04': {
      const columns = p.views.length === 1 ? 1 : 2, rows = Math.ceil(p.views.length / columns);
      if (!['shared-world-scene', 'split', 'four-view', 'six-view'].includes(p.layout) || p.displayTranslation.some(x => x !== 0)) throw new Error('unsupported composition contract');
      spec.surfaces = ['left', 'right'].map(h => corticalSurface(cortex, h));
      spec.views = p.views.map((name, i) => {
        let pose = cameraFor(name, 'explicit', [size[0] / columns, size[1] / rows]);
        if (p.navigation && (p.navigation.links === 'all' || p.navigation.sourceView === name)) {
          pose = cameraAction(pose, { azimuthDegrees: p.navigation.deltaAzimuthDegrees });
        }
        return { id: name, viewport: [(i % columns) / columns, Math.floor(i / columns) / rows, 1 / columns, 1 / rows],
          camera: pose, surfaces: spec.surfaces.filter(s => name.startsWith('left') ? s.hemisphere === 'left' : name.startsWith('right') ? s.hemisphere === 'right' : true).map(s => s.id) };
      });
      spec.camera = spec.views[0].camera;
      break;
    }
    case 'C12':
      throw new Error(`${family} requires composition/publication host actions; single-view adapter cannot qualify it`);
    case 'C05': {
      const s = one(), source = cortex.left;
      s.layers.push(p.fill === 'parcel-scalar' ? scalar('parcels', source.labels.map(l => p.parcelValues[l] ?? 'NaN'), { range: [-3, 3], palette })
        : { id: 'parcels', kind: 'labels', labels: source.labels, definitions: p.labelDefinitions.filter(d => !p.ignoredLabels.includes(d.id)), defaultColor: 0x999999 });
      const labels = p.outline.subset ? source.labels.map(l => p.outline.subset.includes(l) ? 1 : 0) : source.labels;
      s.layers.push({ id: 'boundaries', kind: 'outline', labels, color: p.outline.color, width: p.outline.widthPixels, halo: p.outline.halo,
        haloColor: p.outline.haloColor, haloWidth: p.outline.haloWidthPixels, offset: p.outline.surfaceOffsetMm });
      spec.surfaces = [s];
      break;
    }
    case 'C06': case 'C08': {
      const s = one(), source = cortex.left, annotation = family === 'C06' ? p : p.annotation;
      s.annotations = annotation.anchorIndices.map((index, i) => ({ id: annotation.stableIds[i], vertex: source.anchors[index], radius: annotation.radiusMm,
        colorOn: annotation.colorOn, colorOff: annotation.colorOff, active: p.selected === annotation.stableIds[i] || p.hovered === annotation.stableIds[i] }));
      if (p.showCrosshair) s.crosshair = { vertex: source.anchors[0], options: { size: p.crosshair.sizeMm, color: p.crosshair.color }, label: p.label };
      s.morph = { vertices: source.geometryVariant.vertices, weight: p.morphWeight };
      if (family === 'C08') s.layers.push({ id: 'temporal', kind: 'temporal', frames: source.fields.time.map(f => f.map(Math.fround)), times: p.timesSeconds, range: [-3, 3], palette });
      spec.camera = cameraAction(camera, { azimuthDegrees: p.orbitDegrees ?? 0 });
      spec.surfaces = [s];
      break;
    }
    case 'C07':
      spec.culling = p.culling;
      spec.surfaces = p.surfaces.map(s => ({ id: s.id, hemisphere: 'left', vertices: square2tri.vertices.map((v, i) => [v[0], v[1], s.vertexZ[i]]), faces: square2tri.faces,
        alpha: p.surfaceAlpha, layers: [{ id: 'rgba', kind: 'rgba', visible: p.layersVisible, values: square2tri.vertices.map((_, i) => [...s.rgba.slice(0, 3), p.transparentVertices.includes(i) ? 0 : s.rgba[3]]) }],
        ...(p.planeEnabled ? { clip: { normal: p.plane.normal, point: p.plane.normal.map(x => x * p.plane.offset / p.plane.normal.reduce((a, v) => a + v * v, 0)), flip: p.flip } } : {}) }));
      break;
    case 'C09':
      spec.surfaces = (p.bothHemispheres ? ['left', 'right'] : ['left']).map(h => {
        const s = corticalSurface(cortex, h), source = cortex[h];
        s.layers.push({ id: 'network', kind: 'network', presentation: 'signed-absolute-overlay', edges: p.anchorEdgeIndices.map(([a, b, weight]) => ({ source: source.anchors[a], target: source.anchors[b], weight })),
          colorMap: p.signedPalette, weightRange: [-1, 1], threshold: p.thresholdAbsolute, renderMode: 'tube', tubeRadius: p.widthMm / 2,
          tubeRadiusScale: p.scaleWidthByAbsoluteWeight, showNodes: p.showNodes, nodeRadius: p.nodeRadiusMm, nodeColor: 0x999999, topN: p.topN });
        return s;
      });
      break;
    case 'C10': case 'C11':
      throw new Error(`${family} requires a key/flatmap host adapter; surface-only capture cannot qualify it`);
    default: throw new Error(`unknown family ${family}`);
  }
  const scientific = spec.surfaces.map(s => ({ surface: s.id, vertices: s.vertices.length, faces: s.faces.length,
    landmarks: (s.vertices.length > 100 ? cortex[s.hemisphere].anchors : s.vertices.map((_, i) => i)).map(vertex => ({ vertex,
      screen: landmarkProjection(s.morph ? s.vertices[vertex].map((x, a) => x + s.morph.weight * (s.morph.vertices[vertex][a] - x)) : s.vertices[vertex], spec.camera).map((x, i) => x * size[i]) })) }));
  return { spec, scientific, semantics: 'checkpoint endpoint; interaction replay must be separately qualified' };
}

/** Numeric checkpoint contracts. A contract describes required work; it is
 * neither a capture receipt nor permission to mark an unsupported case passed. */
import { cameraFor, exportSizes, galleryControls, referenceProfiles } from './fixtures.mjs';

export const palette = [[0, 0, .8], [0, .8, 1], [1, 1, .2], [.8, 0, 0]];
const signed = { range: [-2, 3], threshold: [-.25, .25], palette, opacity: .85 };
const scalar = {
  sequential: { values: [0, .25, .75, 1], mapping: { ...signed, range: [0, 1], threshold: [0, 0] } },
  signed: { values: [-2, -.5, .5, 3], mapping: signed },
  'threshold-equality': { values: [-.25, -.2500000298023224, .25, .2500000298023224], mapping: signed },
  'zero-width': { values: [-1, 0, 0, 1], mapping: { ...signed, threshold: [0, 0] } },
  'NaN-mask': { values: ['NaN', -1, 1, 'NaN'], mapping: signed, nonfiniteEncoding: 'NaN string decoded to Float32 NaN' },
  'sharp-edge': { values: [-1, 0, -1, 0], mapping: signed,
    edgePolicy: 'reference visibility-sign boundary halfway between shown and masked vertices; distinct from scalar isocontours' }
};
const volume = {
  dims: [8, 9, 10, 3], voxelToWorld: [1, 0, 0, -3, 0, 1, 0, -4, 0, 0, 1, -5, 0, 0, 0, 1],
  matrixOrder: 'row-major', values: 'i + 2*j + 3*k + 10*t', depthOffsetsMm: [0], reducer: 'mean',
  outside: 'transparent', nearestTie: 'floor(x + .5)', mapping: { ...signed, range: [0, 65], threshold: [0, 0] }
};
const compose = {
  layerOrder: ['curvature', 'scalar-a', 'scalar-b'],
  scalarA: { values: [-2.5, -2.5, 2.5, 2.5], range: [-2.5, 2.5], threshold: [-.35, .35], opacity: .85 },
  scalarB: { values: [1, 1, -1, -1], range: [-1, 1], threshold: [0, 0], opacity: .35 },
  palette: [[0, 0, 1], [1, 0, 0]]
};
const labelDefinitions = [
  { id: 0, color: 0x999999 }, { id: 1, color: 0xe45756 }, { id: 2, color: 0x4c78a8 },
  { id: 3, color: 0xf2cf5b }, { id: 4, color: 0x54a24b }
];
const panelViews = ['leftLateral', 'rightLateral', 'leftMedial', 'rightMedial', 'dorsal', 'ventral'];
const anatomy = {
  initial: {}, cold: {}, orbit: { azimuthDegrees: 20, elevationDegrees: 10 },
  'camera-only': { azimuthDegrees: 20, elevationDegrees: 10 },
  pan: { worldTranslation: [0, 12, 7] }, zoom: { eyeDistanceScale: .8 }, resize: { size: [450, 600] }
};
const annotation = {
  radiusMm: 2.5, colorOn: 0xff00ff, colorOff: 0x0066ff, anchorIndices: [0, 1, 2, 3],
  stableIds: ['marker-a', 'marker-b', 'marker-c', 'marker-d'], label: 'Landmark A',
  crosshair: { sizeMm: 12, color: 0xff00ff }, occlusion: 'depth-tested'
};
const checkpoints = {
  C01: ['initial', 'cold', 'orbit', 'camera-only', 'pan', 'zoom', 'resize'],
  C02: [...Object.keys(scalar), 'nearest', 'linear', 'frame-change'],
  C03: ['normal', 'additive', 'multiply', 'opacity', 'visibility', 'reorder', 'time-style'],
  C04: ['bilateral', 'split', 'four-view', 'six-view', 'linked-camera', 'independent-camera', 'screen', 'export-1x', 'export-2x'],
  C05: ['categorical', 'continuous-parcel', 'exterior-roi', 'halo', 'medial-wall', 'selected-pick'],
  C06: ['multiple', 'selected', 'hover', 'crosshair-label', 'orbit', 'geometry-change'],
  C07: ['axis-plane', 'oblique-plane', 'flipped-side', 'hidden-pick', 'translucent-overlap', 'zero-alpha', 'empty-stack'],
  C08: ['frame-0', 'interpolated', 'pause-resume', 'pial', 'midpoint', 'inflated'],
  C09: ['signed', 'weighted-width', 'node-glyph', 'threshold', 'top-n', 'bilateral'],
  C10: ['continuous', 'diverging', 'categorical', 'effect-confidence', 'window-change', 'threshold-change', 'palette-change'],
  C11: ['hover', 'pick', 'polygon', 'lasso', 'linked-marker', 'linked-time'],
  C12: ['restore', 'selection', 'time', 'camera', 'layers', 'live', 'export-1x', 'export-2x']
};

function settings(family, checkpoint, rowId) {
  if (!checkpoints[family]?.includes(checkpoint)) throw new Error(`unknown checkpoint ${family}/${checkpoint}`);
  switch (family) {
    case 'C01': return { views: panelViews, projections: ['perspective', 'orthographic'],
      profiles: ['observedDefault', 'controlledUnlit'], action: anatomy[checkpoint] };
    case 'C02':
      return rowId.endsWith('volume')
        ? { volume: { ...volume, interpolation: checkpoint === 'nearest' ? 'nearest' : 'linear', frame: checkpoint === 'frame-change' ? 1 : 0 } }
        : scalar[checkpoint];
    case 'C03': return {
      ...compose, blendModeB: ['additive', 'multiply'].includes(checkpoint) ? checkpoint : 'normal',
      ...(checkpoint === 'opacity' ? { opacityB: .6 } : {}),
      ...(checkpoint === 'visibility' ? { visibleB: false } : {}),
      ...(checkpoint === 'reorder' ? { layerOrder: ['curvature', 'scalar-b', 'scalar-a'] } : {}),
      ...(checkpoint === 'time-style' ? { frame: 1, opacityB: .6, valuesB: [-1, 1, -1, 1] } : {})
    };
    case 'C04': return {
      layout: checkpoint === 'bilateral' ? 'shared-world-scene' : checkpoint === 'split' ? 'split' :
        checkpoint === 'six-view' ? 'six-view' : 'four-view',
      views: checkpoint === 'bilateral' ? ['dorsal'] : panelViews.slice(0, checkpoint === 'six-view' ? 6 : checkpoint === 'split' ? 2 : 4),
      sharedWorldFrame: 'fsaverage5-pial-RAS-mm', displayTranslation: [0, 0, 0],
      navigation: ['linked-camera', 'independent-camera'].includes(checkpoint)
        ? { sourceView: 'leftLateral', deltaAzimuthDegrees: 15, links: checkpoint === 'linked-camera' ? 'all' : 'none' } : null,
      exportSize: checkpoint === 'export-2x' ? exportSizes.manuscript : exportSizes.screen
    };
    case 'C05': return {
      labelDefinitions, ignoredLabels: [0], parcelValues: { 1: -3, 2: -1, 3: 1, 4: 3 },
      fill: checkpoint === 'continuous-parcel' ? 'parcel-scalar' : 'categorical',
      outline: { color: 0x000000, widthPixels: 2, halo: checkpoint === 'halo', haloColor: 0xffffff,
        haloWidthPixels: 2, surfaceOffsetMm: .2, subset: checkpoint === 'exterior-roi' ? [1, 2] : null },
      selectedParcel: checkpoint === 'selected-pick' ? 2 : null,
      zoomSeries: [1, 1.5, 2],
      referenceBoundaryPolicy: 'cross-label mesh edges; independently enumerate topology edges',
      roiBoundaryLabels: checkpoint === 'exterior-roi' ? 'binary membership in parcels 1 or 2; omit internal selected-parcel edges' : 'original parcel labels'
    };
    case 'C06': return { ...annotation, selected: checkpoint === 'selected' ? 'marker-a' : null,
      hovered: checkpoint === 'hover' ? 'marker-b' : null, showCrosshair: checkpoint === 'crosshair-label',
      orbitDegrees: checkpoint === 'orbit' ? 35 : 0, morphWeight: checkpoint === 'geometry-change' ? 1 : 0 };
    case 'C07': return {
      surfaces: [
        { id: 'front', vertexZ: [-.4, .4, -.4, .4], rgba: [1, 0, 0, .5] },
        { id: 'back', vertexZ: [.2, -.2, .2, -.2], rgba: [0, 0, 1, .65] }
      ],
      plane: checkpoint === 'oblique-plane' ? galleryControls.planes.oblique : galleryControls.planes.axisX,
      planeEnabled: !['translucent-overlap', 'zero-alpha', 'empty-stack'].includes(checkpoint),
      flip: checkpoint === 'flipped-side', surfaceAlpha: checkpoint === 'translucent-overlap' ? .5 : 1,
      transparentVertices: checkpoint === 'zero-alpha' ? [0, 2] : [],
      layersVisible: checkpoint !== 'empty-stack', pickPointWorld: [-.5, 0, 0],
      culling: 'double-sided', depthWrite: true, alphaDiscard: 1 / 255
    };
    case 'C08': return {
      frames: 3, timesSeconds: [0, 1, 2], interpolation: 'scalar-before-mapping',
      timeSeconds: checkpoint === 'interpolated' ? .5 : checkpoint === 'pause-resume' ? 1.25 : 0,
      playback: checkpoint === 'pause-resume' ? [{ play: true, advanceSeconds: .75 }, { play: false, advanceSeconds: .5 }, { play: true, advanceSeconds: .5 }] : [],
      morphWeight: checkpoint === 'midpoint' ? .5 : checkpoint === 'inflated' ? 1 : 0,
      morphTarget: 'synthetic-expansion-1.2; not anatomical inflation', annotation
    };
    case 'C09': return {
      anchorEdgeIndices: [[0, 1, .8], [1, 2, -.6], [2, 3, .4], [3, 0, -.2]],
      widthMm: 1, scaleWidthByAbsoluteWeight: checkpoint === 'weighted-width', nodeRadiusMm: 2,
      showNodes: checkpoint === 'node-glyph', thresholdAbsolute: checkpoint === 'threshold' ? .5 : 0,
      topN: checkpoint === 'top-n' ? 2 : 4, signedPalette: [[0, 0, 1], [1, 0, 0]],
      bothHemispheres: checkpoint === 'bilateral'
    };
    case 'C10': return {
      mapping: { ...signed, range: checkpoint === 'window-change' ? [-1, 2] : signed.range,
        threshold: checkpoint === 'threshold-change' ? [-.5, .5] : signed.threshold,
        palette: checkpoint === 'palette-change' ? [...palette].reverse() : palette },
      values: [-2, -.5, .5, 3], labelDefinitions, effect: [-2, -2, 3, 3], confidence: [0, 1, 0, 1],
      key: checkpoint === 'effect-confidence' ? 'bivariate' : checkpoint === 'categorical' ? 'categorical' : 'continuous',
      keySizePixels: [240, 160], label: 'Synthetic value', ticks: [-2, 0, 3]
    };
    case 'C11': return {
      flatProjection: 'xy; contain; equal-scale', paddingPixels: 20,
      triangleFill: '#d9d9d9', stroke: '#888888', selectedVertex: 0, hoveredVertex: 3,
      polygon: galleryControls.polygonRoi, boundaryInclusion: 'inside-or-edge',
      lasso: [[-1.1, -1.1], [.1, -1.1], [.1, .1], [-1.1, .1], [-1.1, -1.1]],
      timeSeconds: checkpoint === 'linked-time' ? 1 : 0, linkedMarker: checkpoint === 'linked-marker'
    };
    case 'C12': return {
      views: panelViews.slice(0, 4), title: 'Surface display parity', orientation: ['L lateral', 'R lateral', 'L medial', 'R medial'],
      legend: { label: 'Synthetic value', range: [-3, 3], palette }, transparentBackground: true,
      exportSize: checkpoint === 'export-2x' ? exportSizes.manuscript : exportSizes.screen,
      timeSeconds: checkpoint === 'time' ? 1 : .5, selectedAnchor: 1,
      cameraDeltaDegrees: checkpoint === 'camera' ? 20 : 0,
      layerOrder: checkpoint === 'layers' ? ['scalar', 'curvature'] : ['curvature', 'scalar'],
      roundTrip: ['restore', 'live', 'export-1x', 'export-2x'].includes(checkpoint)
    };
    default: throw new Error(`unknown gallery family ${family}`);
  }
}

const elements = {
  C01: ['surface-mask', 'landmarks', 'curvature'], C02: ['surface-mask', 'mapped-values', 'threshold-mask'],
  C03: ['surface-mask', 'layer-a-mask', 'layer-b-mask', 'composited-rgba'],
  C04: ['panel-order', 'hemisphere-masks', 'instance-picks'],
  C05: ['parcel-fills', 'boundary-segments', 'halo-mask', 'medial-wall-mask'],
  C06: ['marker-a', 'marker-b', 'marker-c', 'marker-d', 'crosshair', 'label'],
  C07: ['front-surface', 'back-surface', 'clip-mask', 'pick-rejection'],
  C08: ['scalar-values', 'morphed-landmarks', 'persistent-marker'],
  C09: ['edge-set', 'signed-edge-colors', 'edge-widths', 'node-glyphs'],
  C10: ['mapped-values', 'legend-colors', 'legend-labels'], C11: ['flat-triangles', 'selection', 'roi-mask', 'linked-marker'],
  C12: ['panel-order', 'orientation', 'title', 'shared-legend', 'restored-selection']
};

export function galleryContracts(manifest) {
  if (!manifest?.requiredMatrix?.length) throw new Error('empty gallery');
  const ids = new Set();
  return manifest.requiredMatrix.flatMap(row => row.checkpoints.map(checkpoint => {
    const id = `${row.id}/${checkpoint}`;
    if (ids.has(id)) throw new Error(`duplicate checkpoint ${id}`);
    ids.add(id);
    const parameters = settings(row.family, checkpoint, row.id);
    if (!parameters || (row.family === 'C01' && !parameters.action)) throw new Error(`missing checkpoint contract ${row.id}/${checkpoint}`);
    const cortical = row.fixture.startsWith('fsaverage5');
    const size = row.route === 'publication export' || ['C04', 'C12'].includes(row.family)
      ? (checkpoint === 'export-2x' ? exportSizes.manuscript : exportSizes.screen)
      : ['C02', 'C03', 'C07', 'C10'].includes(row.family) ? [512, 512] : [600, 450];
    const count = parameters.views?.length ?? 1;
    const tiled = ['C04', 'C12'].includes(row.family);
    const columns = !tiled || count === 1 ? 1 : count === 6 ? 3 : 2;
    const rows = tiled ? Math.ceil(count / columns) : 1;
    const panelSize = [size[0] / columns, size[1] / rows];
    return {
      rowId: row.id, checkpoint, fixture: row.fixture, productRoute: row.route,
      viewport: { origin: [0, 0], size, dpr: 1, originConvention: 'top-left' },
      camera: cameraFor(cortical ? 'leftLateral' : 'analytic-dorsal', 'explicit', size),
      profile: referenceProfiles.controlledUnlit, parameters,
      ...(parameters.views ? { panelCameras: parameters.views.map((view, index) => ({
        view, viewport: { origin: tiled ? [(index % columns) * panelSize[0], Math.floor(index / columns) * panelSize[1]] : [0, 0], size: panelSize },
        perspective: cameraFor(view, 'explicit', panelSize),
        orthographic: cameraFor(view, 'explicit', panelSize, 'orthographic')
      })) } : {}),
      requiredElements: elements[row.family],
      metricPolicy: { minimumForegroundIoU: .995, maximumChannelError: 2, maximumLandmarkPixels: 1,
        smallElementRequired: true, calibration: 'pending; cannot pass until reference repeatability is recorded' }
    };
  }));
}

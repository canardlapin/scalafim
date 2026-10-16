/** Public SurfViewJS adapter. Inputs are fixture values, never candidate output.
 * Unsupported operations throw; this adapter must not silently omit an element.
 * Each mount owns one viewer and therefore one shared depth domain.
 */
function finiteVector(value, name) {
  if (!Array.isArray(value) || value.length !== 3 || value.some(x => !Number.isFinite(x))) {
    throw new Error(`${name} must contain three finite coordinates`);
  }
}

export function applyReferenceCamera(viewer, camera) {
  for (const name of ['eye', 'target', 'up']) finiteVector(camera[name], name);
  const { fovDegrees, near, far, aspect } = camera;
  if (![near, far, aspect].every(Number.isFinite) || !(near > 0 && far > near && aspect > 0)) {
    throw new Error('invalid camera projection');
  }
  if (camera.projection === 'perspective' && viewer.camera.isPerspectiveCamera) {
    if (!(fovDegrees > 0 && fovDegrees < 180)) throw new Error('invalid perspective field of view');
    Object.assign(viewer.camera, { fov: fovDegrees, near, far, aspect });
  } else if (camera.projection === 'orthographic' && viewer.camera.isOrthographicCamera) {
    const { left, right, bottom, top } = camera;
    if (![left, right, bottom, top].every(Number.isFinite) || !(right > left && top > bottom)) throw new Error('invalid orthographic bounds');
    Object.assign(viewer.camera, { left, right, bottom, top, near, far, zoom: camera.zoom ?? 1 });
  } else throw new Error('projection type requires a matching public reference camera');
  viewer.camera.position.set(...camera.eye);
  viewer.camera.up.set(...camera.up);
  viewer.cameraControls.target.set(...camera.target);
  viewer.camera.lookAt(...camera.target);
  viewer.cameraControls.update();
  viewer.camera.updateProjectionMatrix();
  viewer.camera.updateMatrixWorld();
}

/** Opt-in public-client synchronization for ordinary noninstanced markers.
 * The pin's moveAnnotation reads base vertices. Mesh.getVertexPosition reads
 * the reference's own current morph; no candidate output enters this route.
 */
export function syncReferenceAnnotationAnchors(viewer, three, annotationIds) {
  for (const {referenceId} of annotationIds) {
    const record = viewer.getAnnotation(referenceId);
    const mesh = record && viewer.getSurface(record.surfaceId)?.mesh;
    const count = mesh?.geometry.getAttribute('position')?.count;
    if (!mesh || !Number.isInteger(count) || count < 1 || record.marker.parent !== mesh || !Number.isInteger(record.vertexIndex) || record.vertexIndex < 0 || record.vertexIndex >= count) {
      throw new Error('follow-morph client requires an ordinary surface-child marker and valid vertex');
    }
    const local = mesh.getVertexPosition(record.vertexIndex, new three.Vector3());
    if (![local.x,local.y,local.z].every(Number.isFinite)) throw new Error('nonfinite reference morphed marker');
    record.marker.position.copy(local);
    record.position.copy(local);
  }
  viewer.requestRender();
}

function applyProfile(ref, viewer, profile) {
  if (profile.name === 'observed-default') return;
  if (!['controlled-unlit', 'controlled-lit'].includes(profile.name)) {
    throw new Error(`unsupported reference material profile ${profile.name}`);
  }
  const preset = ref.getStylePreset('default');
  viewer.setHeadlight(false);
  viewer.applyStylePreset({
    ...preset,
    background: { css: '#ffffff', clearColor: 0xffffff, clearAlpha: 1 },
    material: { ...preset.material, baseColor: 0xcccccc, materialType: 'phong', alpha: 1 },
    curvature: { brightness: .5, contrast: .5, smoothness: 1 },
    lighting: {
      ...preset.lighting, ambientColor: 0xffffff, directionalColor: 0xffffff,
      ambientIntensity: profile.ambientIntensity,
      directionalIntensity: profile.directionalIntensity,
      directionalPosition: profile.direction ?? [0, 0, 1], fillIntensity: 0,
      rimStrength: 0, silhouetteDarkening: 0, thresholdOutline: 0, overlayEmission: 0
    }
  });
}

function makeLayer(ref, three, specification, registerPalette) {
  const { id, kind, opacity = 1, visible = true, blendMode = 'normal' } = specification;
  const options = { opacity, visible, blendMode };
  const colorMap = () => new ref.ColorMap(specification.palette, {
    range: specification.range, threshold: specification.threshold ?? [0, 0]
  });
  switch (kind) {
    case 'scalar':
      return new ref.DataLayer(id, new Float32Array(specification.values), null, colorMap(), {
        ...options, range: specification.range, threshold: specification.threshold ?? [0, 0]
      });
    case 'temporal':
      return new ref.TemporalDataLayer(id, specification.frames.map(frame => new Float32Array(frame)),
        specification.times, colorMap(), {
          ...options, range: specification.range, threshold: specification.threshold ?? [0, 0]
        });
    case 'rgba':
      return new ref.RGBALayer(id, new Float32Array(specification.values.flat()), options);
    case 'volume': {
      const { dims, frame, voxelValues, voxelToWorld, mapping } = specification;
      if (specification.matrixOrder !== 'row-major' || specification.reducer !== 'mean' ||
          JSON.stringify(specification.depthOffsetsMm) !== '[0]' || specification.outside !== 'transparent' ||
          specification.nearestTie !== 'floor(x + .5)' || !['nearest', 'linear'].includes(specification.interpolation)) {
        throw new Error('unsupported reference volume sampling contract');
      }
      if (!Number.isInteger(frame) || frame < 0 || frame >= dims[3] || voxelValues.length !== dims.slice(0, 3).reduce((a, b) => a * b, 1)) throw new Error('invalid volume frame');
      // Three Matrix4.set takes row-major arguments. The source affine and
      // x-fastest voxel data are not inferred from either rendered surface.
      const affine = new three.Matrix4().set(...voxelToWorld);
      return new ref.VolumeProjectionLayer(id, new Float32Array(voxelValues), dims.slice(0, 3), {
        ...options, opacity: mapping.opacity, range: mapping.range, threshold: mapping.threshold,
        // The reference bundle may own a different Three constructor. Plain
        // column-major elements avoid cross-bundle instanceof ambiguity.
        colormap: registerPalette(mapping.palette), affineMatrix: affine.toArray(),
        sampling: specification.interpolation, projectionMode: 'vertex',
        // This finite sentinel is outside the frozen analytic source range;
        // measured zero remains valid, rather than being hidden by the default.
        fillValue: -Number.MAX_VALUE
      });
    }
    case 'labels':
      return new ref.LabelLayer(id, {
        ...options, labels: specification.labels, labelDefs: specification.definitions,
        defaultColor: specification.defaultColor
      });
    case 'outline':
      return new ref.OutlineLayer(id, {
        ...options, roiLabels: specification.labels, color: specification.color,
        width: specification.width, halo: specification.halo,
        haloColor: specification.haloColor, haloWidth: specification.haloWidth,
        offset: specification.offset, roiSubset: specification.subset ?? null
      });
    case 'network':
      return new ref.ConnectivityLayer(id, specification.edges, {
        ...options, colorMap: Array.isArray(specification.colorMap) ? registerPalette(specification.colorMap) : specification.colorMap, weightRange: specification.weightRange,
        threshold: specification.threshold, renderMode: specification.renderMode,
        tubeRadius: specification.tubeRadius, tubeRadiusScale: specification.tubeRadiusScale,
        showNodes: specification.showNodes, nodeRadius: specification.nodeRadius,
        nodeColor: specification.nodeColor, topN: specification.topN
      });
    default:
      throw new Error(`reference layer adapter is missing: ${kind}`);
  }
}

/** Express the frozen signed palette and |weight| radius using public layers.
 * A single ConnectivityLayer maps both color and width differently. Selection
 * remains the reference's own threshold/top-N operation, before decomposition.
 * Repeated endpoint spheres are opaque and coincident; no geometry is invented.
 */
export function makeReferenceNetworkLayers(ref, specification, registerPalette) {
  if (specification.renderMode !== 'tube' || specification.opacity !== undefined && specification.opacity !== 1 ||
      !Array.isArray(specification.colorMap) || specification.colorMap.length !== 2) {
    throw new Error('signed network client requires opaque tubes and two signed colors');
  }
  const selector = new ref.ConnectivityLayer(`${specification.id}/selector`, specification.edges, {
    threshold: specification.threshold, topN: specification.topN, showNodes: false
  });
  let selected;
  try { selected = selector.getFilteredEdges(); } finally { selector.dispose(); }
  return selected.map((edge, index) => {
    const color = specification.colorMap[edge.weight < 0 ? 0 : 1];
    const radius = specification.tubeRadius * (specification.tubeRadiusScale ? Math.abs(edge.weight) : 1);
    if (!(radius > 0)) throw new Error('zero-radius reference tube is not representable');
    return new ref.ConnectivityLayer(`${specification.id}/edge-${index}`, [edge], {
      opacity: 1, visible: specification.visible ?? true,
      colorMap: registerPalette([color, color]), weightRange: specification.weightRange,
      threshold: 0, topN: 0, renderMode: 'tube', tubeRadius: radius, tubeRadiusScale: false,
      showNodes: specification.showNodes, nodeRadius: specification.nodeRadius, nodeColor: specification.nodeColor
    });
  });
}

/** Multiple actual public viewers, composed at their declared panel bounds.
 * Shared input meshes retain their scientific coordinates in every panel. */
function mountReferenceComposition(ref, three, root, specification, client) {
  const { width, height, views, surfaces } = specification;
  const mounted = [];
  root.style.cssText = `width:${width}px;height:${height}px;position:relative`;
  try {
    for (const input of views) {
      const [nx, ny, nw, nh] = input.viewport;
      const bounds = [nx * width, ny * height, nw * width, nh * height];
      if (!bounds.every(Number.isInteger)) throw new Error('reference panel boundaries require integer pixels');
      const child = document.createElement('div'); root.append(child);
      const { views: omitted, ...single } = specification;
      const view = mountReferenceView(ref, three, child, { ...single, width: bounds[2], height: bounds[3],
        camera: input.camera, surfaces: surfaces.filter(s => input.surfaces.includes(s.id)) }, client);
      child.style.position = 'absolute'; child.style.left = `${bounds[0]}px`; child.style.top = `${bounds[1]}px`;
      mounted.push({ id: input.id, bounds, view });
    }
  } catch (error) {
    for (const { view } of mounted) view.dispose();
    root.replaceChildren(); throw error;
  }
  const canvas = document.createElement('canvas'); canvas.width = width; canvas.height = height;
  const context = canvas.getContext('2d');
  if (!context) { for (const { view } of mounted) view.dispose(); throw new Error('composition Canvas2D unavailable'); }
  let disposed = false;
  return {
    pick(x, y) {
      const panel = mounted.find(({ bounds: [px, py, w, h] }) => x >= px && x < px + w && y >= py && y < py + h);
      if (!panel) return null;
      const hit = panel.view.pick(x - panel.bounds[0], y - panel.bounds[1]);
      return hit && { ...hit, view: panel.id };
    },
    capture() {
      if (disposed) throw new Error('reference composition is disposed');
      context.fillStyle = '#ffffff'; context.fillRect(0, 0, width, height);
      const receipts = mounted.map(({ id, bounds, view }) => {
        const result = view.capture();
        context.drawImage(view.viewer.renderer.domElement, bounds[0], bounds[1]);
        return { id, bounds, ...result.receipt };
      });
      return { image: canvas.toDataURL('image/png'), receipt: { ...receipts[0], width, height,
        views: receipts, materials: receipts.flatMap(r => r.materials.map(m => ({ ...m, view: r.id }))),
        volumeSamples: receipts.flatMap(r => r.volumeSamples), route: 'public viewer per panel' } };
    },
    dispose() { if (!disposed) { disposed = true; for (const { view } of mounted) view.dispose(); root.replaceChildren(); } }
  };
}

export function mountReferenceView(ref, three, root, specification, client = {}) {
  if (specification.views) return mountReferenceComposition(ref, three, root, specification, client);
  const { width, height, surfaces, camera, profile } = specification;
  if (![width, height].every(x => Number.isInteger(x) && x > 0)) throw new Error('invalid viewport');
  if (!surfaces?.length) throw new Error('reference view requires surfaces');
  if (specification.culling !== undefined && specification.culling !== 'double-sided') throw new Error('unsupported culling policy');
  if (new Set(surfaces.map(s => s.id)).size !== surfaces.length) throw new Error('duplicate surface instance');
  root.style.cssText = `width:${width}px;height:${height}px;flex:none;position:relative`;
  const viewer = new ref.NeuroSurfaceViewer(root, width, height, {
    backgroundColor: 0xffffff, useShaders: false, initialZoom: 4, controlType: 'trackball',
    ...(profile.name !== 'observed-default' ? { headlight: false } : {})
  }, 'left_dorsal');
  const layers = new Map();
  const palettes = [];
  const registry = ref.ColorMap.getPresetMaps();
  const registerPalette = colors => {
    if (!Array.isArray(colors) || colors.length < 2 || colors.some(row => !Array.isArray(row) || row.length !== 3 || row.some(x => !Number.isFinite(x) || x < 0 || x > 1))) throw new Error('invalid fixture palette');
    let serial = 0, name;
    do { name = `__scalafim_qualification_${serial++}`; } while (Object.hasOwn(registry, name));
    registry[name] = colors.map(row => [...row]); palettes.push(name);
    return name;
  };
  const releasePalettes = () => { for (const name of palettes) delete registry[name]; };
  const annotationIds = [];
  let networkDrawOrder = 1;
  let disposed = false;
  try {
    if (viewer.initializationFailed) throw new Error('reference WebGL initialization failed');
    viewer.resize(width, height, { dpr: 1 });
    for (const input of surfaces) {
      if (input.crosshair?.label) throw new Error('reference crosshair text label adapter is not implemented');
      const geometry = new ref.SurfaceGeometry(new Float32Array(input.vertices.flat()),
        new Uint32Array(input.faces.flat()), input.hemisphere ?? 'left',
        input.curvature ? new Float32Array(input.curvature) : undefined, false);
      const options = {
        showCurvature: input.showCurvature ?? false,
        ...(input.alpha !== undefined ? { alpha: input.alpha } : {}),
        ...(input.baseColor !== undefined ? { baseColor: input.baseColor } : {}),
        ...(input.curvature ? { curvature: new Float32Array(input.curvature) } : {}),
        ...(profile.name !== 'observed-default'
          ? { specularColor: 0x000000, emissive: 0x000000, emissiveIntensity: 0 } : {})
      };
      const surface = input.morph
        ? new ref.MorphableSurface(geometry, options)
        : new ref.MultiLayerNeuroSurface(geometry, options);
      if (input.morph) {
        surface.addMorphTarget('synthetic-expansion', new Float32Array(input.morph.vertices.flat()));
        surface.setMorphWeight('synthetic-expansion', input.morph.weight);
      }
      for (const layerSpec of input.layers ?? []) {
        const signedNetwork = layerSpec.kind === 'network' && layerSpec.presentation === 'signed-absolute-overlay';
        const made = signedNetwork ? makeReferenceNetworkLayers(ref, layerSpec, registerPalette)
          : [makeLayer(ref, three, layerSpec, registerPalette)];
        for (const layer of made) surface.addLayer(layer);
        if (signedNetwork) {
          // Public scene objects retain input painter order after decomposing
          // one instanced layer into independently colored fixed-radius layers.
          made.forEach((layer, index) => {
            for (const object of layer.getGroup().children) {
              if (object.name === `connectivity-tubes-${layer.id}`) object.renderOrder = networkDrawOrder + index;
              else if (object.name === `connectivity-nodes-${layer.id}`) object.renderOrder = networkDrawOrder + made.length + index;
              else throw new Error('unexpected public connectivity display object');
            }
          });
          networkDrawOrder += made.length * 2;
        }
        layers.set(`${input.id}/${layerSpec.id}`, signedNetwork ? made : made[0]);
      }
      if (input.placement) surface.mesh.position.set(...input.placement);
      viewer.addSurface(surface, input.id);
      if (input.clip) {
        const plane = surface.getClipPlane('x');
        plane.setFromNormalAndPoint(new three.Vector3(...input.clip.normal), new three.Vector3(...input.clip.point));
        if (input.clip.flip) plane.flip();
        surface.enableClipPlane('x');
      }
      for (const marker of input.annotations ?? []) {
        const id = viewer.addAnnotation(input.id, marker.vertex, { fixtureId: marker.id }, {
          radius: marker.radius, colorOn: marker.colorOn, colorOff: marker.colorOff, active: marker.active
        });
        if (id === null) throw new Error(`reference rejected annotation ${marker.id}`);
        annotationIds.push({ fixtureId: marker.id, referenceId: id, surface: input.id, vertex: marker.vertex });
      }
      if (input.crosshair) viewer.showCrosshair(input.id, input.crosshair.vertex, input.crosshair.options);
    }
    applyProfile(ref, viewer, profile);
    // Style presets set surface alpha; the specimen's explicit alpha wins.
    for (const input of surfaces) {
      const surface = viewer.surfaces.get(input.id);
      if (input.alpha !== undefined) surface.updateConfig({ alpha: input.alpha });
      if (input.layers?.some(layer => layer.thresholdEdges !== undefined)) {
        surface.setShading({ thresholdEdges: input.layers.some(layer => layer.thresholdEdges === true) });
      }
    }
    if (camera.projection === 'orthographic') {
      // Public field substitution is exercised by upstream viewer-fit tests.
      // Reinstall through setupControls so scheduler listeners follow the new
      // camera. No surface additions/fitting or composer use after this point.
      viewer.stop();
      viewer.cameraControls.dispose();
      viewer.camera = new three.OrthographicCamera();
      viewer.setupControls();
    }
    applyReferenceCamera(viewer, camera);
    for (const input of surfaces) for (const layer of input.layers ?? []) {
      if (layer.kind === 'temporal') {
        const time = specification.timeSeconds ?? 0;
        const lo = Math.min(layer.times.length - 1, Math.max(0, layer.times.findLastIndex(x => x <= time)));
        const hi = Math.min(layer.times.length - 1, lo + 1);
        layers.get(`${input.id}/${layer.id}`).setTime(lo, hi, hi === lo ? 0 : (time - layer.times[lo]) / (layer.times[hi] - layer.times[lo]));
      }
    }
    viewer.start();
  } catch (error) {
    try { viewer.dispose(); } finally { releasePalettes(); }
    throw error;
  }
  return {
    viewer,
    camera(next) { applyReferenceCamera(viewer, next); },
    resize(width, height, nextCamera) {
      viewer.resize(width, height, { dpr: 1 });
      applyReferenceCamera(viewer, nextCamera);
      viewer.cameraControls.handleResize?.();
    },
    time(surface, layer, frameA, frameB, alpha) {
      const temporal = layers.get(`${surface}/${layer}`);
      if (!(temporal instanceof ref.TemporalDataLayer)) throw new Error('temporal layer missing');
      temporal.setTime(frameA, frameB, alpha);
    },
    pick(x, y) {
      const bounds = viewer.renderer.domElement.getBoundingClientRect();
      const hit = viewer.pick({ x: bounds.left + x, y: bounds.top + y, useGPU: false });
      return hit.surfaceId === null ? null : { surface: hit.surfaceId, vertex: hit.vertexIndex,
        world: hit.point ? [hit.point.x, hit.point.y, hit.point.z] : null };
    },
    capture() {
      if (disposed) throw new Error('reference view is disposed');
      if (client.followMorphAnnotations === true) syncReferenceAnnotationAnchors(viewer, three, annotationIds);
      viewer.render();
      // Read in the render turn: the drawing buffer need not be preserved.
      const image = viewer.renderer.domElement.toDataURL('image/png');
      const gl = viewer.renderer.getContext();
      const debug = gl.getExtension('WEBGL_debug_renderer_info');
      return { image, receipt: {
        camera: viewer.getCameraState(), viewMatrix: [...viewer.camera.matrixWorldInverse.elements],
        projectionMatrix: [...viewer.camera.projectionMatrix.elements], matrixOrder: 'column-major',
        width: viewer.renderer.domElement.width, height: viewer.renderer.domElement.height,
        profile, annotationIds,
        annotationPolicy: {followMorph:client.followMorphAnnotations===true,
          route:client.followMorphAnnotations===true?'public-client checkpoint synchronization':'native annotation anchoring',
          limitations:'ordinary noninstanced markers; checkpoint synchronization does not emit annotation:moved or qualify continuous animation',
          morphWeights:surfaces.filter(s=>s.morph).map(s=>({surface:s.id,weight:s.morph.weight}))},
        markerCenters: annotationIds.map(({fixtureId, referenceId, surface, vertex}) => {
          const record = viewer.getAnnotation(referenceId);
          if (!record?.marker.parent) throw new Error('annotation world readout requires a noninstanced public marker');
          const p = record.marker.getWorldPosition(new three.Vector3());
          return {id:fixtureId,surface,vertex,world:[p.x,p.y,p.z]};
        }),
        networks: surfaces.flatMap(surface => (surface.layers ?? []).filter(layer => layer.kind === 'network' && layer.presentation === 'signed-absolute-overlay').map(layer => ({
          surface: surface.id, layer: layer.id, route: 'public filtered per-edge ConnectivityLayer',
          selected: layers.get(`${surface.id}/${layer.id}`).flatMap(value => value.getFilteredEdges()),
          colors: layers.get(`${surface.id}/${layer.id}`).map(value => [...value.getEdgeColors()]),
          configurations: layers.get(`${surface.id}/${layer.id}`).map(value => value.toStateJSON()),
          renderOrders: layers.get(`${surface.id}/${layer.id}`).flatMap(value => value.getGroup().children.map(object => ({ name: object.name, order: object.renderOrder })))
        }))),
        surfaceIds: [...viewer.surfaces.keys()],
        volumeSamples: surfaces.flatMap(surface => (surface.layers ?? []).filter(layer => layer.kind === 'volume').map(layer => ({
          surface: surface.id, layer: layer.id,
          values: surface.vertices.map((_, vertex) => layers.get(`${surface.id}/${layer.id}`).sampleValueAtVertex(vertex))
        }))),
        materials: [...viewer.surfaces].map(([id, surface]) => ({ id,
          depthWrite: surface.mesh.material.depthWrite, alphaDiscard: surface.mesh.material.alphaTest,
          transparent: surface.mesh.material.transparent, side: surface.mesh.material.side,
          renderOrder: surface.mesh.renderOrder })),
        outputColorSpace: viewer.renderer.outputColorSpace, toneMapping: viewer.renderer.toneMapping,
        antialias: gl.getContextAttributes().antialias,
        renderer: String(gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER))
      } };
    },
    dispose() { if (!disposed) { disposed = true; try { viewer.dispose(); } finally { releasePalettes(); root.replaceChildren(); } } }
  };
}

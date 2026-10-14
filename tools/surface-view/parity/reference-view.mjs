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
  if (camera.projection !== 'perspective') {
    throw new Error('reference viewer exposes a PerspectiveCamera; an orthographic public host adapter is not qualified');
  }
  for (const name of ['eye', 'target', 'up']) finiteVector(camera[name], name);
  const { fovDegrees, near, far, aspect } = camera;
  if (!(fovDegrees > 0 && fovDegrees < 180 && near > 0 && far > near && aspect > 0)) {
    throw new Error('invalid camera projection');
  }
  Object.assign(viewer.camera, { fov: fovDegrees, near, far, aspect });
  viewer.camera.position.set(...camera.eye);
  viewer.camera.up.set(...camera.up);
  viewer.cameraControls.target.set(...camera.target);
  viewer.camera.lookAt(...camera.target);
  viewer.cameraControls.update();
  viewer.camera.updateProjectionMatrix();
  viewer.camera.updateMatrixWorld();
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

function makeLayer(ref, specification) {
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
        ...options, colorMap: specification.colorMap, weightRange: specification.weightRange,
        threshold: specification.threshold, renderMode: specification.renderMode,
        tubeRadius: specification.tubeRadius, tubeRadiusScale: specification.tubeRadiusScale,
        showNodes: specification.showNodes, nodeRadius: specification.nodeRadius,
        nodeColor: specification.nodeColor, topN: specification.topN
      });
    default:
      throw new Error(`reference layer adapter is missing: ${kind}`);
  }
}

export function mountReferenceView(ref, three, root, specification) {
  const { width, height, surfaces, camera, profile } = specification;
  if (![width, height].every(x => Number.isInteger(x) && x > 0)) throw new Error('invalid viewport');
  if (!surfaces?.length) throw new Error('reference view requires surfaces');
  if (new Set(surfaces.map(s => s.id)).size !== surfaces.length) throw new Error('duplicate surface instance');
  root.style.cssText = `width:${width}px;height:${height}px;flex:none;position:relative`;
  const viewer = new ref.NeuroSurfaceViewer(root, width, height, {
    backgroundColor: 0xffffff, useShaders: false, initialZoom: 4,
    ...(profile.name !== 'observed-default' ? { headlight: false } : {})
  }, 'left_dorsal');
  const layers = new Map();
  const annotationIds = [];
  let disposed = false;
  try {
    if (viewer.initializationFailed) throw new Error('reference WebGL initialization failed');
    viewer.resize(width, height, { dpr: 1 });
    for (const input of surfaces) {
      const geometry = new ref.SurfaceGeometry(new Float32Array(input.vertices.flat()),
        new Uint32Array(input.faces.flat()), input.hemisphere ?? 'left',
        input.curvature ? new Float32Array(input.curvature) : undefined, false);
      const options = {
        showCurvature: input.showCurvature ?? false,
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
        const layer = makeLayer(ref, layerSpec);
        surface.addLayer(layer);
        layers.set(`${input.id}/${layerSpec.id}`, layer);
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
    applyReferenceCamera(viewer, camera);
  } catch (error) {
    viewer.dispose();
    throw error;
  }
  return {
    viewer,
    camera(next) { applyReferenceCamera(viewer, next); },
    time(surface, layer, frameA, frameB, alpha) {
      const temporal = layers.get(`${surface}/${layer}`);
      if (!(temporal instanceof ref.TemporalDataLayer)) throw new Error('temporal layer missing');
      temporal.setTime(frameA, frameB, alpha);
    },
    capture() {
      if (disposed) throw new Error('reference view is disposed');
      viewer.render();
      // Read in the render turn: the drawing buffer need not be preserved.
      const image = viewer.renderer.domElement.toDataURL('image/png');
      const gl = viewer.renderer.getContext();
      const debug = gl.getExtension('WEBGL_debug_renderer_info');
      return { image, receipt: {
        camera: viewer.getCameraState(), viewMatrix: [...viewer.camera.matrixWorldInverse.elements],
        projectionMatrix: [...viewer.camera.projectionMatrix.elements], matrixOrder: 'column-major',
        width: viewer.renderer.domElement.width, height: viewer.renderer.domElement.height,
        profile, annotationIds, surfaceIds: [...viewer.surfaces.keys()],
        outputColorSpace: viewer.renderer.outputColorSpace, toneMapping: viewer.renderer.toneMapping,
        antialias: gl.getContextAttributes().antialias,
        renderer: String(gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER))
      } };
    },
    dispose() { if (!disposed) { disposed = true; viewer.dispose(); root.replaceChildren(); } }
  };
}

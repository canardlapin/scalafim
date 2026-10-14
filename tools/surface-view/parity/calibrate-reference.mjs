/** Reference-only repeatability and independent mapping probes. These cannot
 * qualify a candidate, a lit folded surface, or an untested platform. */
export async function captureReferenceCalibration({ ref, three, mount }) {
  const camera = {
    projection: 'perspective', eye: [0, 0, 4], target: [0, 0, 0], up: [0, 1, 0],
    fovDegrees: 35, near: .01, far: 1000, aspect: 1
  };
  const profile = {
    name: 'controlled-unlit', ambientIntensity: Math.PI, directionalIntensity: 0,
    direction: [0, 0, 1], specular: 0, emissive: 0, emissiveIntensity: 0, base: 204, lightFrame: 'world'
  };
  const vertices = [[-1, -1, 0], [1, -1, 0], [-1, 1, 0], [1, 1, 0]];
  const faces = [[0, 1, 2], [1, 3, 2]];
  const specimens = [
    { id: 'red', rgba: [1, 0, 0, 1], expected: [255, 0, 0, 255] },
    { id: 'green', rgba: [0, 1, 0, 1], expected: [0, 255, 0, 255] },
    { id: 'blue', rgba: [0, 0, 1, 1], expected: [0, 0, 255, 255] },
    { id: 'gray', rgba: [.8, .8, .8, 1], expected: [204, 204, 204, 255] },
    { id: 'curvature-negative', curvature: -.5, expected: [191, 191, 191, 255] },
    { id: 'curvature-positive', curvature: .5, expected: [64, 64, 64, 255] },
    { id: 'curvature-zero', curvature: 0, expected: [128, 128, 128, 255] },
    { id: 'gray-half-light', rgba: [128 / 255, 128 / 255, 128 / 255, 1], expected: [92, 92, 92, 255],
      profile: { ...profile, name: 'controlled-lit', ambientIntensity: Math.PI * .5 } },
    { id: 'white-half-opacity-over-black', rgba: [1, 1, 1, .5], baseColor: 0, expected: [128, 128, 128, 255] },
    { id: 'black-white-gradient', rgbaVertices: [[0, 0, 0, 1], [1, 1, 1, 1], [0, 0, 0, 1], [1, 1, 1, 1]],
      oracle: 'decode-vertices-then-linear-interpolation-then-encode' }
  ];
  const results = [];
  for (const specimen of specimens) {
    const repeats = [];
    for (let repeat = 0; repeat < 2; repeat++) {
      const root = document.createElement('div');
      document.body.append(root);
      let handle;
      try {
        handle = mount(ref, three, root, {
          width: 256, height: 256, camera, profile: specimen.profile ?? profile,
          surfaces: [{ id: 'calibration', vertices, faces,
            baseColor: specimen.baseColor,
            showCurvature: specimen.curvature !== undefined,
            curvature: specimen.curvature === undefined ? undefined : Array(4).fill(specimen.curvature),
            layers: specimen.rgba || specimen.rgbaVertices
              ? [{ id: 'rgba', kind: 'rgba', values: specimen.rgbaVertices ?? Array(4).fill(specimen.rgba) }] : []
          }]
        });
        repeats.push(handle.capture());
      } finally { handle?.dispose(); root.remove(); }
    }
    results.push({ id: specimen.id, expectedInteriorRgba: specimen.expected,
      oracle: specimen.oracle ?? 'constant-independent-rgba', repeats });
  }
  return { schema: 'scalafim.reference-calibration.v1', camera, profile,
    interior: { x: 96, y: 96, width: 64, height: 64 }, specimens: results };
}

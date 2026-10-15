import test from 'node:test';
import assert from 'node:assert/strict';
import { makeReferenceNetworkLayers, syncReferenceAnnotationAnchors } from './reference-view.mjs';

test('signed network client delegates selection and expresses fixed signed colors and radii', () => {
  const created = [], palettes = [];
  const retained = [{ source: 7, target: 8, weight: -.8 }, { source: 2, target: 3, weight: .4 }];
  class ConnectivityLayer {
    constructor(id, edges, config) { Object.assign(this, { id, edges, config }); created.push(this); }
    getFilteredEdges() { return retained.map(edge => ({ ...edge })); }
    dispose() { this.disposed = true; }
  }
  const layers = makeReferenceNetworkLayers({ ConnectivityLayer }, { id: 'network', edges: [
    ...retained, { source: 1, target: 2, weight: .1 }], threshold: .3, topN: 2,
    colorMap: [[1,0,0],[0,0,1]], weightRange: [-1,1], tubeRadius: .5, tubeRadiusScale: true,
    renderMode: 'tube', showNodes: true, nodeRadius: 1, nodeColor: 0x999999
  }, colors => { palettes.push(colors); return `palette-${palettes.length}`; });
  assert.equal(created[0].config.threshold, .3);
  assert.equal(created[0].config.topN, 2);
  assert.equal(created[0].disposed, true);
  assert.deepEqual(layers.map(layer => layer.edges), retained.map(edge => [edge]));
  assert.deepEqual(layers.map(layer => layer.config.tubeRadius), [.4,.2]);
  assert.ok(layers.every(layer => layer.config.tubeRadiusScale === false && layer.config.opacity === 1));
  assert.deepEqual(palettes, [[[1,0,0],[1,0,0]],[[0,0,1],[0,0,1]]]);
});

test('public follow-morph client keeps local coordinates and rejects instanced placeholders', () => {
  class Vector3 { constructor(x=0,y=0,z=0){Object.assign(this,{x,y,z});} copy(v){Object.assign(this,v);return this;} }
  const mesh={geometry:{getAttribute:()=>({count:2})},getVertexPosition:(id,p)=>p.copy({x:3,y:4,z:5})};
  const record={surfaceId:'left',vertexIndex:1,position:new Vector3(),marker:{parent:mesh,position:new Vector3()}};
  let requested=0;const viewer={getAnnotation:()=>record,getSurface:()=>({mesh}),requestRender:()=>requested++};
  syncReferenceAnnotationAnchors(viewer,{Vector3},[{referenceId:'a'}]);
  assert.deepEqual({...record.position},{x:3,y:4,z:5});assert.deepEqual({...record.marker.position},{x:3,y:4,z:5});assert.equal(requested,1);
  record.marker.parent=null;assert.throws(()=>syncReferenceAnnotationAnchors(viewer,{Vector3},[{referenceId:'a'}]),/ordinary/);
  record.marker.parent=mesh;record.vertexIndex=2;assert.throws(()=>syncReferenceAnnotationAnchors(viewer,{Vector3},[{referenceId:'a'}]),/valid vertex/);
});

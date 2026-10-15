import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';

const canonical = value => Array.isArray(value) ? `[${value.map(canonical).join(',')}]` : value && typeof value === 'object'
  ? `{${Object.keys(value).sort().map(key => `${JSON.stringify(key)}:${canonical(value[key])}`).join(',')}}` : JSON.stringify(value);
const hash = value => createHash('sha256').update(canonical(value)).digest('hex');
const fail = message => { throw new TypeError(message); };
const viewport = value => {
  if (!value || !Array.isArray(value.size) || value.size.length !== 2 || !value.size.every(x => Number.isFinite(x) && x > 0)) fail('invalid frozen viewport');
  return { origin: value.origin ?? [0, 0], size: [...value.size], dpr: value.dpr ?? 1, originConvention: value.originConvention ?? 'top-left' };
};
const manifestSizes = text => [...String(text ?? '').matchAll(/(\d+)x(\d+)/g)].map(match => [+match[1], +match[2]]);
const distinct = values => [...new Map(values.map(value => [canonical(value), value])).values()];

export function enumerateQualificationContracts({ manifest, frozen }) {
  if (!manifest || !Array.isArray(manifest.requiredMatrix) || !frozen || !Array.isArray(frozen.contracts)) fail('missing manifest or frozen contracts');
  if (createHash('sha256').update(JSON.stringify(frozen.contracts)).digest('hex') !== frozen.contractsSha256) fail('frozen contracts hash mismatch');
  const rows = new Map();
  for (const row of manifest.requiredMatrix) {
    if (!row?.id || !row.fixture || !Array.isArray(row.checkpoints) || !row.checkpoints.length) fail('invalid manifest row');
    if (rows.has(row.id)) fail(`duplicate manifest row ${row.id}`);
    rows.set(row.id, row);
  }
  const parents = new Set();
  const expectedParents = new Set([...rows.values()].flatMap(row => row.checkpoints.map(checkpoint => `${row.id}/${checkpoint}`)));
  const seen = new Set(); const variants = [];
  for (const contract of frozen.contracts) {
    const row = rows.get(contract?.rowId);
    if (!row || !row.checkpoints.includes(contract.checkpoint) || contract.fixture !== row.fixture || contract.productRoute !== row.route) fail(`unknown or malformed frozen contract ${contract?.rowId}/${contract?.checkpoint}`);
    const parent = `${contract.rowId}/${contract.checkpoint}`;
    if (parents.has(parent)) fail(`duplicate frozen checkpoint ${parent}`);
    parents.add(parent);
    const baseViewport = viewport(contract.viewport);
    const sizes = distinct([baseViewport.size, ...manifestSizes(row.viewport)]).map((size, index) => ({ ...baseViewport, size, source: index === 0 ? 'frozen' : 'manifest' }));
    const bundle = contract.rowId.startsWith('C01') ? (() => {
      const views = contract.parameters?.views, projections = contract.parameters?.projections, profiles = contract.parameters?.profiles;
      if (![views, projections, profiles].every(x => Array.isArray(x) && x.length && new Set(x).size === x.length)) fail('C01 requires nonempty distinct views, projections, profiles');
      if (views.some(v => !['leftLateral', 'leftMedial', 'rightLateral', 'rightMedial', 'dorsal', 'ventral'].includes(v)) || projections.some(p => !['perspective', 'orthographic'].includes(p)) || profiles.some(p => !['observedDefault', 'controlledUnlit'].includes(p))) fail('unknown C01 bundle input');
      return views.flatMap(view => projections.flatMap(projection => profiles.map(profile => ({ view, projection, profile, requirement: 'required' })))).concat(
        views.flatMap(view => projections.map(projection => ({ view, projection, profile: 'controlledLit', requirement: 'supplementary-controlled-lit-anatomy' })))
      );
    })() : Array.isArray(contract.parameters?.zoomSeries)
      ? (() => {
        const zooms = contract.parameters.zoomSeries;
        if (!zooms.length || zooms.some(x => !Number.isFinite(x) || x <= 0) || new Set(zooms).size !== zooms.length) fail('invalid zoom series');
        return zooms.map(zoom => ({ zoom, requirement: 'required' }));
      })() : [{ requirement: 'required' }];
    for (const variant of bundle) for (const effectiveViewport of sizes) {
      const input = { ...contract, viewport: effectiveViewport, bundle: variant };
      const id = `${contract.rowId}/${contract.checkpoint}/${hash(input).slice(0, 16)}`;
      if (seen.has(id)) fail(`duplicate variant ${id}`); seen.add(id);
      const expectedElements = Object.fromEntries((contract.requiredElements ?? []).map(id => [id, { expected: 'present' }]));
      const absence = [];
      if (contract.rowId.startsWith('C05') && !contract.parameters.outline?.halo) absence.push('halo-mask');
      if (contract.rowId.startsWith('C06') && !contract.parameters.showCrosshair) absence.push('crosshair', 'label');
      if (contract.rowId.startsWith('C09') && !contract.parameters.showNodes) absence.push('node-glyphs');
      if (contract.rowId.startsWith('C11') && !contract.parameters.linkedMarker) absence.push('linked-marker');
      if (contract.rowId.startsWith('C07') && !contract.parameters.planeEnabled) absence.push('clip-mask');
      for (const id of absence) if (expectedElements[id]) expectedElements[id] = { expected: 'absent' };
      if (!Object.keys(expectedElements).length) fail(`missing required elements for ${contract.rowId}/${contract.checkpoint}`);
      const ticks = contract.parameters?.ticks, range = contract.parameters?.mapping?.range;
      const diagnostics = contract.rowId.startsWith('C10') && contract.checkpoint === 'window-change' && Array.isArray(ticks) && Array.isArray(range) && ticks.some(t => t < range[0] || t > range[1])
        ? [{ code: 'legend-ticks-outside-window', ticks: [...ticks], range: [...range], verdict: 'contract-diagnostic' }] : [];
      variants.push({ id, parent: { rowId: contract.rowId, checkpoint: contract.checkpoint }, requirement: variant.requirement,
        effectiveInput: input, effectiveInputHash: hash(input), expectedElements, diagnostics, frozenContractsSha256: frozen.contractsSha256 });
      if (diagnostics.length) {
        const positive = { ...input, parameters: { ...input.parameters, ticks: [-1, 0, 2] }, erratum: 'valid-window-key-20261015' };
        variants.push({ id: `${contract.rowId}/${contract.checkpoint}/${hash(positive).slice(0, 16)}`, parent: { rowId: contract.rowId, checkpoint: contract.checkpoint },
          requirement: 'required-positive-erratum', originalInputHash: hash(input), effectiveInput: positive, effectiveInputHash: hash(positive), expectedElements,
          diagnostics: [], frozenContractsSha256: frozen.contractsSha256 });
      }
    }
  }
  for (const parent of expectedParents) if (!parents.has(parent)) fail(`missing frozen checkpoint ${parent}`);
  return { sourceHashes: frozen.sourceHashes, frozenContractsSha256: frozen.contractsSha256, variants };
}

export async function loadQualificationContracts(directory = new URL('.', import.meta.url)) {
  const [manifest, frozen] = await Promise.all(['manifest.json', 'frozen-inputs.json'].map(async name => JSON.parse(await readFile(new URL(name, directory), 'utf8'))));
  return enumerateQualificationContracts({ manifest, frozen });
}

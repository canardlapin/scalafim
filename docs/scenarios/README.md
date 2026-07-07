# Scenario Manifest

`manifest.json` is the active registry for executable ScalaFIM scenario tests.
It records which scenarios are release-relevant, where they live, what reference
strategy they use, which platforms they run on, and which focused commands
verify them.

The manifest is not yet a full receipt store. Per-scenario receipts and a
manifest gate will come after more than one external fixture exists.

Validate the manifest JSON with:

```sh
python -m json.tool docs/scenarios/manifest.json >/tmp/scalafim-scenario-manifest.json
```

Regenerate the current fmrimod/Nilearn fixture with:

```sh
python tools/scenarios/export_fit_public_f_contrast_fixture.py
```

Pass `--fmrimod-root /path/to/fmrimod` if the sibling checkout is not at
`~/code/pycode/fmrimod`.

Check that its JSON and shared Scala fixture are current with:

```sh
python tools/scenarios/export_fit_public_f_contrast_fixture.py --check
```

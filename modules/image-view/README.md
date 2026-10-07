# Image viewer model updates

`model.updated(ViewerModelUpdate.ReorderLayers(ids))` promotes the listed ids
in that order and preserves the relative order of unmentioned layers. Duplicate
or unknown ids are errors. `ReplaceLayer(layer)` replaces an existing id with
an already typed `SliceLayer`; construct it with the desired colorizer and the
same source/sampling when recoloring. No unchecked colorizer cast is needed.

On Canvas, `controller.updateModel(update)` adopts the edit atomically while
preserving its session and runtime caches. It rejects incompatible timepoints,
windows or thresholds without reading source data. Reordering reuses unchanged
layer rasters. A replacement is sampled/colorized afresh because samples retain
their colorizer; other layers remain cached. External hosts can call
`session.validateModel(nextModel)` before adopting the same pure model update.

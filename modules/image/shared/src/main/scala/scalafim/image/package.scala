package scalafim

package object image:
  export image4s.SomeSampleSpace
  export image4s.locus.GridDomain
  export GridDomainOps.*

  export SomeNeuroVolume.{
    label,
    values,
    space,
    ndim,
    apply,
    wholeCanonical,
    materializedCanonical,
    plane,
    gridToIndex,
    indexToGrid,
    indexToVoxel,
    valueAtCanonicalOrdinal,
    copyToCanonicalArray,
    asMatrix,
    asLogical,
    asMask,
    toSeries,
    concatenate,
    mapValues,
    map,
    mapVoxels,
    zipExact,
    traverseValues
  }

  export SomeNeuroSeries.{
    label,
    values,
    space,
    ndim,
    nVolumes,
    apply,
    wholeCanonical,
    voxelTimeMatrix,
    materializedCanonical,
    volume,
    volumeAt,
    valueAtCanonicalOrdinal,
    valueAtVoxelOrdinal,
    copyToCanonicalArray,
    gridToIndex,
    indexToGrid,
    asMatrix,
    subArray,
    timeSeries,
    selectTimes,
    concatenate,
    mapValues,
    map,
    mapVoxels,
    mapSamples,
    zipExact
  }

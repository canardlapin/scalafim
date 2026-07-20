package scalafim.graph

/** A dense coordinate in one particular [[VertexBasis]].
  *
  * `VertexIx` is not stable domain identity. Reindexing or taking an induced
  * basis may change it while preserving the vertex key.
  */
opaque type VertexIx = Int

object VertexIx:
  private[graph] def unsafe(value: Int): VertexIx =
    value

  extension (index: VertexIx)
    inline def toInt: Int = index

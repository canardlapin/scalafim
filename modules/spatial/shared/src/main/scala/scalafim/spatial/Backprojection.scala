package scalafim.spatial

import gale.linalg.DMat

/** Data in a view's domain carried back to its root domain by the adjoint of the view's operator.
  *
  * `operator` is the root-to-view operator (its path, provenance and `usedInverses` say how the view was routed);
  * `field` holds `operator^T * data` as a materialized field in the root domain. The adjoint is an operator-level
  * transpose, not a geometric inverse, so it exists for every compiled view.
  */
final case class Backprojection(field: Field, data: DMat, operator: SpatialOperator)

object Backprojection:
  /** neurofunctor `backproject`: compile the view's operator from its own intent (route, sampling, selected rows,
    * inverse policy) and apply its adjoint to `data`, a `view.sampleCount x k` matrix. Observations pass through.
    */
  def of(view: Field, data: DMat, graph: SpatialGraph): Either[SpatialError, Backprojection] =
    if !view.isView then Left(SpatialError.FieldIsRoot(view.domain))
    else if data.rows != view.sampleCount then Left(SpatialError.FieldShapeMismatch(view.sampleCount, data.rows))
    else
      val intent = view.plan.intent
      val request =
        CompileRequest.forRows(
          source = view.root,
          target = view.domain,
          rowSelection = intent.rowSelection,
          routing = intent.routing,
          sampling = intent.sampling,
          allowInverses = intent.allowInverses
        )
      for
        operator <- OperatorCompiler.compile(graph, request)
        _ <-
          if operator.rows == data.rows then Right(())
          else Left(SpatialError.FieldShapeMismatch(operator.rows, data.rows))
        backprojected <- operator.map.transposeApplyTo(data).left.map(SpatialQc.linearError)
      yield Backprojection(
        Field.fromMatrix(view.root, backprojected, s"${view.data.label}:backprojected"),
        backprojected,
        operator
      )

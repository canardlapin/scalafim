package scalafim.fmri.fit.profile

/** Test-only access to the actual native ML evaluator, never a replacement kernel. */
object PhrfMlEnergyTestAccess:
  final case class Evidence(valueEnergy: Double, jetEnergy: Double, determinant: Double,
      readoutEnergy: Double, gradient: Vector[Double], hessian: Vector[Double])

  def evaluate(preparation: TrialBandedPreparation, response: Array[Double],
      coordinates: Vector[Vector[Double]], sigma2: Double): Either[String, Vector[Evidence]] =
    for
      bank <- preparation.objective(NodeGrid(preparation.basis.family.chart, Vector(3, 3))).left.map(_.message)
      backend <- TrialBandedMlBackend.make(bank).result.left.map(_.message)
      values <- coordinates.foldLeft[Either[String, Vector[Evidence]]](Right(Vector.empty)): (acc, at) =>
        for
          done <- acc
          _ <- backend.pointAt(response).result.left.map(_.message)
          value <- backend.valueAt(at).result.left.map(_.message)
          jet <- backend.jetAt(at).result.left.map(_.message)
          criterion <- CriterionJet.assemble(sigma2, CriterionJet.Input.TrialMl(jet)).left.map(_.message)
          readout <- backend.exactReadout(at).left.map(_.message)
        yield done :+ Evidence(value.raw.energy, jet.raw.jet.energy, jet.determinant.value,
          readout.penalizedEnergy, criterion.minimizationJet.gradient, criterion.minimizationJet.hessian)
    yield values

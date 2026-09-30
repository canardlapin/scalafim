package scalafim.fmri.fit.scenarios

import gale.linalg.{DMat, DVec}

object PublicFContrastNilearnFixture:
  val scenarioId: String = "fit.public-f-contrast.v1"
  val source: String = "Algorithm mirrored from fmrimod cross_testing.fitlins_parity.fit_fitlins_reference_ols plus nilearn run_glm/compute_contrast"
  val residualDegreesOfFreedom: Int = 5
  val columnNames: Vector[String] = Vector("task", "base_constant", "nuis#01_1")
  val task: Vector[Double] = Vector(-1.5, -1.0, -0.25, 0.75, 1.25, -0.5, 0.5, 1.75)
  val motion: Vector[Double] = Vector(0.20000000000000001, -0.40000000000000002, 0.69999999999999996, -0.59999999999999998, 0.10000000000000001, 0.90000000000000002, -0.80000000000000004, 0.29999999999999999)
  val responseRows: Vector[Vector[Double]] =
    Vector(
      Vector(-1.53, 0.03500000000000001),
      Vector(-0.35999999999999999, -1.05),
      Vector(0.75, -1.1125),
      Vector(3.2000000000000002, -3.4175),
      Vector(3.9699999999999998, -3.5274999999999999),
      Vector(0.10999999999999996, -0.58999999999999997),
      Vector(2.8400000000000003, -3.335),
      Vector(4.8600000000000003, -3.9225000000000003)
    )
  val design: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(-1.5, 1.0, 0.20000000000000001),
        Vector(-1.0, 1.0, -0.40000000000000002),
        Vector(-0.25, 1.0, 0.69999999999999996),
        Vector(0.75, 1.0, -0.59999999999999998),
        Vector(1.25, 1.0, 0.10000000000000001),
        Vector(-0.5, 1.0, 0.90000000000000002),
        Vector(0.5, 1.0, -0.80000000000000004),
        Vector(1.75, 1.0, 0.29999999999999999)
      )
    )
  val coefficients: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(1.9901073580940001, -1.257996075683),
        Vector(1.5010944639099999, -1.9999462859149999),
        Vector(-0.3971576734408, 0.8439159074982)
      )
    )
  val residualVariance: DVec =
    DVec.fromSeq(Vector(0.0012534054660130001, 0.001259992221444))
  val taskTEstimates: DVec =
    DVec.fromSeq(Vector(1.9901073580940001, -1.257996075683))
  val taskTStandardErrors: DVec =
    DVec.fromSeq(Vector(0.01204297113892, 0.01207457310925))
  val taskTStatistics: DVec =
    DVec.fromSeq(Vector(165.25052955269999, -104.1855529219))
  val taskTPValues: DVec =
    DVec.fromSeq(Vector(1.5396399820520001E-10, 1.5446772749779999E-09))
  val taskFStatistics: DVec =
    DVec.fromSeq(Vector(27307.73751744, 10854.62943764))
  val taskFPValues: DVec =
    DVec.fromSeq(Vector(1.5396399820520001E-10, 1.544677332319E-09))
  val taskAndMotionFStatistics: DVec =
    DVec.fromSeq(Vector(14672.71923174, 6955.6679559539998))

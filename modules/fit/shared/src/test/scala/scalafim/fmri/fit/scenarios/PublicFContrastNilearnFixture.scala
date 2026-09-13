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
        Vector(1.9901073580939042, -1.2579960756832522),
        Vector(1.5010944639103019, -1.9999462859145065),
        Vector(-0.39715767344078484, 0.84391590749824763)
      )
    )
  val residualVariance: DVec =
    DVec.fromSeq(Vector(0.0012534054660126161, 0.0012599922214435815))
  val taskTEstimates: DVec =
    DVec.fromSeq(Vector(1.9901073580939042, -1.2579960756832522))
  val taskTStandardErrors: DVec =
    DVec.fromSeq(Vector(0.01204297113891918, 0.012074573109252463))
  val taskTStatistics: DVec =
    DVec.fromSeq(Vector(165.25052955266895, -104.18555292188996))
  val taskTPValues: DVec =
    DVec.fromSeq(Vector(1.5396399820519448E-10, 1.5446772749783122E-09))
  val taskFStatistics: DVec =
    DVec.fromSeq(Vector(27307.737517437519, 10854.629437639933))
  val taskFPValues: DVec =
    DVec.fromSeq(Vector(1.5396399820519425E-10, 1.5446773323185949E-09))
  val taskAndMotionFStatistics: DVec =
    DVec.fromSeq(Vector(14672.719231742898, 6955.6679559539834))

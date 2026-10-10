package scalafim.fmri.laws.profile

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scalafim.fmri.design.hrf.{KernelBasisCompilation, TrialDesignLowering}
import scalafim.fmri.fit.profile.*

/** Small complete attempted-cohort diagnostic, not throughput or production admission. */
object TrialReferenceDecodeMain:
  private def hash(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
  private def doubles(values: Vector[Double]): ujson.Value =
    ujson.Arr.from(values.map(v => if v.isFinite then ujson.Num(v) else ujson.Str(v.toString)))

  def main(args: Array[String]): Unit =
    require(args.length == 1, "packet-directory")
    val dir = Path.of(args(0))
    val protocolBytes = Files.readAllBytes(dir.resolve("protocol.json"))
    require(
      hash(protocolBytes) == "c8e01ddbcb4b934beae315716589adf3979bb9a7d1c1ba0941deb41c16815a33",
      "frozen protocol changed"
    )
    val protocol = ujson.read(protocolBytes)
    require(protocol("id").str == "explicit-reference-decoder-integration/v1")
    require(protocol("frozenCandidates") == ujson.Arr(1, 6))
    require(protocol("trials") == ujson.Arr(30, 300) && protocol("voxelsPerGeometry").num == 8)
    require(protocol("noiseRatio").num == 0.1 && protocol("decodeBudget").str == "DecodeBudget() defaults")
    val selectedBytes = Files.readAllBytes(dir.getParent.resolve("phrf-reference-bank-20261008/selected.json"))
    require(
      hash(selectedBytes) == "8a60ff8c946f9db8609af8f7ffe6202a9456975640baecd095cd0251faaf5156",
      "frozen bank selection changed"
    )
    val selected = ujson.read(selectedBytes)
    val points = TrialReferencePlacement.points("4x1x2")
    require(selected("name").str == "4x1x2")
    require(selected("coordinates") == ujson.Arr.from(points.coordinates.map(doubles)))
    require(!Files.exists(dir.resolve("diagnostic.json")), "refuse to replace existing measurements")
    val results = Vector(30, 300).map: trials =>
      val f = DecodedTrialCheckpoint.fixture(
        DecodedTrialCheckpoint.Config(
          DecodedTrialCheckpoint.Geometry.B0Dense,
          voxels = 8,
          trials = trials,
          blockSize = 2,
          compilation = KernelBasisCompilation.BlockedPartial(96),
          trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(32)),
          noiseRatio = Some(0.1),
          horizonSeconds = Some(96.0),
          basisMaxRank = 24
        ),
        Some(TrialDomainProposal.family)
      )
      val outputs = f
        .prepareWithReferences(Some(TrialReferenceDecodePolicy(points, Vector(1, 6))))
        .flatMap(_.trialOutputs)
        .fold(e => throw new IllegalArgumentException(e.message), identity)
      val records = Vector.newBuilder[ujson.Value]
      val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
          payload.results.foreach: voxel =>
            val decoded = voxel.selection
            val record = ujson.Obj(
              "voxel" -> voxel.voxelId,
              "status" -> decoded.status.toString,
              "coordinates" -> doubles(decoded.coordinates),
              "startingReference" -> decoded.node,
              "budgetExit" -> decoded.budgetExit.fold[ujson.Value](ujson.Null)(v => ujson.Str(v.toString))
            )
            voxel.output match
              case ProfileTrialOutputOutcome.DecodeRefused(status)     => record("output") = s"DecodeRefused($status)"
              case ProfileTrialOutputOutcome.ReadoutRefused(error)     => record("output") = error.message
              case ProfileTrialOutputOutcome.Emitted(reference, value) =>
                record("output") = "Emitted"
                record("readoutReference") = reference.index
                val oracle = f.oracle(voxel.voxelId, decoded.coordinates)
                val actual = value.trialAmplitudes.get ++ value.nuisanceCoefficients
                record("maxSameBasisQrError") = actual.zip(oracle).map((a, b) => math.abs(a - b)).max
                record("referenceInverseAttempts") = value.work.referenceInverseAttempts.toString
                record("residualCorrections") = value.work.residualCorrections
            records += record
          Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
      val summary = outputs
        .run(new f.Reader, DecodedTrialCheckpoint.request, ProfileTrialReadoutMode.CorrectedReference, sink)
        .fold(e => throw new IllegalArgumentException(e.message), identity)
      val work = summary.progress.trial.get
      ujson.Obj(
        "trials" -> trials,
        "rank" -> f.plan.basis.rank,
        "setupReferenceAttempts" -> summary.setup.bankSetup.get.nodeReferenceAttempts.toString,
        "attemptedVoxels" -> summary.progress.attemptedVoxels,
        "nodeScores" -> summary.progress.decoder.nodeScores.toString,
        "jets" -> summary.progress.decoder.jets.toString,
        "continuousFactors" -> work.continuousFactors.toString,
        "factorAttempts" -> work.attempted.factorAttempts.toString,
        "reconstructedBands" -> work.attempted.reconstructedBands.toString,
        "readoutExactFactors" -> summary.progress.publicReadout.get.numerical.attempted.exactReadoutFactorAttempts.toString,
        "records" -> ujson.Arr.from(records.result())
      )
    val receipt = ujson.Obj(
      "protocolSha256" -> hash(protocolBytes),
      "selectionSha256" -> hash(selectedBytes),
      "productionAdmitted" -> false,
      "results" -> ujson.Arr.from(results)
    )
    val _ = Files.writeString(dir.resolve("diagnostic.json"), ujson.write(receipt, indent = 2) + "\n")

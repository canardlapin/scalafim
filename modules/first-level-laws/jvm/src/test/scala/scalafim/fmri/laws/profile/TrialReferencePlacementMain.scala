package scalafim.fmri.laws.profile

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scalafim.fmri.fit.profile.*

object TrialReferencePlacementMain:
  private val expectedProtocol = "3d68e7e4fe106f877608c09468c3cfd62b97b35ec578347bdf5ab92adc88eda1"
  private def hash(bytes: Array[Byte]): String = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
  private def json(value: Any): ujson.Value = value match
    case v: ujson.Value => v
    case v: scalafim.scenarios.ScenarioResult => ujson.Obj("id" -> v.id, "status" -> v.status.toString,
      "ciPass" -> v.ciPass, "observations" -> ujson.Arr.from(v.observations.map(_.render)), "caveats" -> ujson.Arr.from(v.caveats.map(_.render)))
    case None => ujson.Null
    case Some(v) => json(v)
    case v: Vector[?] => ujson.Arr.from(v.map(json))
    case v: String => ujson.Str(v)
    case v: Double => if v.isFinite then ujson.Num(v) else ujson.Str(v.toString)
    case v: Int => ujson.Num(v)
    case v: Long => ujson.Num(v.toDouble)
    case v: Boolean => ujson.Bool(v)
    case v: Product => ujson.Obj.from(v.productElementNames.zip(v.productIterator).map((k, v) => k -> json(v)))
    case v => throw new IllegalArgumentException(s"unsupported receipt $v")

  def main(args: Array[String]): Unit =
    require(args.length == 2, "packet-directory development|confirmation|stress")
    val dir = Path.of(args(0))
    val protocol = hash(Files.readAllBytes(dir.resolve("protocol.json")))
    require(protocol == expectedProtocol, "protocol changed after freezing")
    def write(name: String, value: ujson.Value): Unit =
      val _ = Files.writeString(dir.resolve(name), ujson.write(value, indent = 2) + "\n")
    val start = System.nanoTime()
    args(1) match
      case "development" =>
        require(!Files.exists(dir.resolve("selected.json")), "selection already frozen")
        val results = TrialReferencePlacement.candidates.map: (name, _) =>
          println(s"Development candidate $name")
          name -> TrialReferencePlacement.run(name, 300, 256, 0, confirm = false, completed = println)
        write("development.json", json(results))
        val selected = results.zipWithIndex.minBy: (entry, index) =>
          val fresh = entry._2.coverage.find(_.cohort == "fresh").get
          (-fresh.correctedPasses, fresh.originalP95, index)
        write("selected.json", ujson.Obj("name" -> selected._1._1, "protocolSha256" -> protocol,
          "developmentSha256" -> hash(Files.readAllBytes(dir.resolve("development.json"))),
          "coordinates" -> json(TrialReferencePlacement.points(selected._1._1).coordinates)))
      case "confirmation" =>
        val selectedBytes = Files.readAllBytes(dir.resolve("selected.json"))
        val selected = ujson.read(selectedBytes)
        require(selected("protocolSha256").str == protocol)
        require(selected("developmentSha256").str == hash(Files.readAllBytes(dir.resolve("development.json"))))
        val name = selected("name").str
        require(selected("coordinates") == json(TrialReferencePlacement.points(name).coordinates))
        val results = Vector(TrialReferencePlacement.run(name, 30, 64, 32, confirm = true, completed = println),
          TrialReferencePlacement.run(name, 300, 256, 64, confirm = true, completed = println))
        write("confirmation.json", ujson.Obj("protocolSha256" -> protocol, "selectionSha256" -> hash(selectedBytes),
          "results" -> json(results), "seconds" -> (System.nanoTime() - start) / 1e9))
      case "stress" =>
        val selected = ujson.read(Files.readAllBytes(dir.resolve("selected.json")))
        require(selected("protocolSha256").str == protocol)
        val bank = TrialReferencePlacement.stressBank(selected("name").str)
        val workers = Vector.fill(8)(bank.newWorker())
        val before = System.nanoTime()
        val raw = Array.tabulate(bank.preparation.rows)(i => math.sin(i * 0.17))
        val encoded = bank.preparation.encodeWhitened(raw).toOption.get
        val worker = workers.head
        worker.pointAt(encoded)
        val jet = new ProfileJetBuffer(3, 3)
        val completed = worker.jetAtNode(0, jet)
        val curvatureSeconds = (System.nanoTime() - before) / 1e9
        // Allocate every curvature worker's on-demand band before measuring.
        workers.tail.foreach: other =>
          other.pointAt(bank.preparation.encodeWhitened(raw).toOption.get)
          require(other.jetAtNode(0, new ProfileJetBuffer(3, 3)))
        val prep = bank.preparation
        import scalafim.fmri.design.{TrialId, ConditionId, ColumnId, ScanIndex}
        import scalafim.fmri.hrf.family.NormalizationRule
        val conditions = Vector.tabulate(prep.conditions)(i => ConditionId.unsafe(s"condition-$i"))
        val axis = ProfileTrialAxis.make(prep.source, prep,
          Vector.tabulate(prep.trials)(i => TrialId.unsafe(s"trial-$i")), conditions,
          Vector.tabulate(prep.trials)(i => conditions(prep.membership.conditionOfTrial(i))),
          Vector.tabulate(prep.nuisanceColumns)(i => ColumnId.unsafe(s"nuisance-$i")),
          Vector.tabulate(prep.rows)(i => ScanIndex.unsafeOneBased(i + 1))).toOption.get
        val readout = ProfileTrialReadout.freeze(bank, axis, bank.points.coordinates.head, 0,
          NormalizationRule.Unnormalised, ProfileTrialReadoutMode.CorrectedReference).toOption.get
        val readoutWorkers = Vector.fill(8)(readout.newWorker())
        val input = ProfileTrialResponse.make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Original, raw).toOption.get
        val result = readoutWorkers.head.evaluate(input, DecodedTrialCheckpoint.request).toOption.get
        val graph = retainedGraph((bank +: workers).toArray[AnyRef])
        val readoutGraph = retainedGraph((Vector[AnyRef](bank) ++ readoutWorkers).toArray)
        val combinedGraph = retainedGraph((Vector[AnyRef](bank) ++ workers ++ readoutWorkers).toArray)
        write("stress.json", ujson.Obj("protocolSha256" -> protocol, "selectedBank" -> selected("name"),
          "rank" -> bank.preparation.basisRank, "bandwidth" -> bank.preparation.bandwidth,
          "preparationBytes" -> bank.preparation.receipt.estimatedBytes,
          "referenceBytes" -> bank.estimatedReferenceBytes, "fullReferenceBytes" -> bank.estimatedFullReferenceBytes,
          "sharedBytes" -> bank.estimatedSharedBytes, "workerBytes" -> bank.estimatedWorkerBytes,
          "reconstructionScratchBytes" -> bank.reconstructionScratchBytes,
          "curvatureWorkersWarmed" -> workers.length,
          "retainedCurvatureScratchBytes" -> workers.map(_.retainedReconstructionScratchBytes).sum,
          "retainedBankScratchBytes" -> bank.retainedReconstructionScratchBytes,
          "scopedEightWorkersBytes" -> (bank.estimatedSharedBytes + 8 * bank.estimatedWorkerBytes),
          "reachableBankAndEightWorkers" -> graph,
          "reachableBankAndEightReadoutWorkers" -> readoutGraph,
          "reachableBankEightCurvatureAndEightReadoutWorkers" -> combinedGraph,
          "readoutWork" -> json(result.work), "terminalJetCompleted" -> completed,
          "terminalWork" -> json(worker.work.snapshot), "terminalSecondsDiagnosticOnly" -> curvatureSeconds,
          "seconds" -> (System.nanoTime() - start) / 1e9))
      case other => throw new IllegalArgumentException(s"unknown mode $other")

  /** Identity-deduplicated reachable graph. The optional test-only Java agent
    * measures actual shallow object sizes; no heuristic JVM layout estimate.
    * Inaccessible instance fields are errors, never silently omitted.
    */
  private def retainedGraph(roots: Array[AnyRef]): ujson.Value =
    val sizeMethod = Class.forName("PhrfMemoryAgent").getMethod("sizeOf", classOf[Object])
    val seen = new java.util.IdentityHashMap[AnyRef, java.lang.Boolean]()
    val pending = new java.util.ArrayDeque[AnyRef]()
    pending.push(roots)
    var bytes = 0L
    var doubleValues = 0L
    while !pending.isEmpty do
      val value = pending.pop()
      if !seen.containsKey(value) then
        seen.put(value, java.lang.Boolean.TRUE)
        bytes += sizeMethod.invoke(null, value).asInstanceOf[java.lang.Long].longValue()
        val cls = value.getClass
        if cls.isArray then
          if cls.getComponentType == java.lang.Double.TYPE then doubleValues += java.lang.reflect.Array.getLength(value)
          if !cls.getComponentType.isPrimitive then
            var i = 0
            while i < java.lang.reflect.Array.getLength(value) do
              val child = java.lang.reflect.Array.get(value, i)
              if child != null then pending.push(child)
              i += 1
        else
          var current: Class[?] = cls
          while current != null do
            current.getDeclaredFields.foreach: field =>
              if !java.lang.reflect.Modifier.isStatic(field.getModifiers) && !field.getType.isPrimitive then
                field.setAccessible(true)
                val child = field.get(value)
                if child != null then pending.push(child)
            current = current.getSuperclass
    ujson.Obj("bytes" -> bytes, "objects" -> seen.size(), "doubleValues" -> doubleValues,
      "scope" -> "all objects reachable from the stated roots, including shared basis/source/response and object overhead; excludes external plans, build and ML determinant temporaries")

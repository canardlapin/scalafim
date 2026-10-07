package scalafim.fmri.design.fixtures

import scalafim.fmri.design.*
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

/** Exact Scala port of the m7b synthetic study event generators.  The DMS
  * `sampleOnset` is explicit source-row parent timing, not reconstructed from
  * its phase schedules.  Reference: model-studio-hillclimb-20261002 m7b.
  */
object ModelStudioEventTimingFixture:
  final case class Study(name: String, model: EventModel, parents: Option[ParentTrialOnsets])

  /** SHA-256 of JSON event rows for seeds 11 and 29, checked directly against
    * `out/m7b/index.html`'s extracted `STUDIES` functions on 2026-10-02.
    */
  val sourceEventHashes: Map[String, String] = Map(
    "block" -> "9f548b2df5eb58ffdc7a34f710b36ebc6b61f00d7352d7e261373f9eee1879ed",
    "gamble" -> "3d505ae5f16854592c3992dab3348282dbee4907b52969397bdea94df9d6dfd0",
    "dms" -> "82ce7e6361b51f11724c3fb4c56e736384991a0b0d37e1da0c7965a72a6d05d7",
    "stop" -> "f2740aeb4e19aa05c1ecbfd3b54fa721200beb3fb61a5c7766b62de10411b170",
    "dense" -> "172114a3f20407320f15eb8473954003d878d31f5a02d30d9533077331235bc1"
  )

  private val seeds = Vector(11, 29)

  private final class Mulberry(initial: Int):
    private var state = initial
    private def imul(left: Int, right: Int): Int = (left.toLong * right.toLong).toInt
    def next(): Double =
      state = state + 0x6D2B79F5
      var value = imul(state ^ (state >>> 15), 1 | state)
      value = (value + imul(value ^ (value >>> 7), 61 | value)) ^ value
      ((value ^ (value >>> 14)).toLong & 0xffffffffL).toDouble / 4294967296.0

  private def shuffle[A](values: Vector[A], random: Mulberry): Vector[A] =
    val out = values.toArray[Any]
    var i = out.length - 1
    while i > 0 do
      val j = (random.next() * (i + 1)).toInt
      val swap = out(i); out(i) = out(j); out(j) = swap
      i -= 1
    out.toVector.asInstanceOf[Vector[A]]

  private def event(values: Vector[String], name: String): Event = Event.factor(values, name)
  private def frame(samples: Int): SamplingFrame = SamplingFrame(blockLens = Seq(samples, samples), tr = Seq(2.0, 2.0))
  private def term(frame: SamplingFrame, tag: String, factors: Vector[(String, Vector[String])], onsets: Vector[Double], durations: Vector[Double], blocks: Vector[Int], phase: Option[PhaseId] = None, provenance: Vector[EventRowProvenance] = Vector.empty): ConvolvedTerm =
    EventTerm(factors.map((name, values) => event(values, name)), onsets.map(Seconds(_)), durations.map(Seconds(_)), blocks, Some(tag), phase, provenance).convolve(Hrfs.SPMG1, frame)
  private def model(frame: SamplingFrame, terms: Vector[ConvolvedTerm]): EventModel = EventModel.build(terms, frame)

  def studies: Vector[Study] = Vector(block, gamble, dms, stop, dense)

  private def block: Study =
    val sampling = frame(180)
    val rows = seeds.zipWithIndex.flatMap { (seed, run) =>
      val base = Vector("faces", "houses", "scrambled")
      val rows = (0 until 4).flatMap { cycle =>
        val rotation = (cycle + (if seed == 11 then 0 else 1)) % 3
        (base.drop(rotation) ++ base.take(rotation)).zipWithIndex.map((condition, local) => (cycle * 3 + local, condition))
      }
      rows.map((index, condition) => (12.0 + index * 28.0, condition, run))
    }
    Study("block", model(sampling, Vector(term(sampling, "blocks", Vector("condition" -> rows.map(_._2)), rows.map(_._1), Vector.fill(rows.size)(16.0), rows.map(_._3)))), None)

  private final case class GambleRow(run: Int, response: String, onset: Double, responseOnset: Double)
  private def gamble: Study =
    val sampling = frame(240)
    val rows = seeds.zipWithIndex.flatMap { (seed, run) =>
      val random = new Mulberry(seed); var time = 6.0
      Vector.tabulate(56) { _ =>
        val gain = 10 + 2 * (random.next() * 16).toInt
        val loss = 5 + (random.next() * 16).toInt
        val accepted = random.next() < 1.0 / (1.0 + math.exp(-(gain - 2 * loss) / 6.0))
        val rt = math.rint((0.8 + random.next() * 1.2) * 100.0) / 100.0
        val row = GambleRow(run, if accepted then "accept" else "reject", time, math.rint((time + rt) * 100.0) / 100.0)
        time += 5.0 + random.next() * 4.0
        row
      }
    }
    Study("gamble", model(sampling, Vector(
      term(sampling, "gamble", Vector.empty, rows.map(_.onset), Vector.fill(rows.size)(3.0), rows.map(_.run)),
      term(sampling, "response", Vector("response" -> rows.map(_.response)), rows.map(_.responseOnset), Vector.fill(rows.size)(0.0), rows.map(_.run))
    )), None)

  private final case class DmsRow(run: Int, trial: Int, load: String, stim: String, sample: Double, delay: Double, delayDuration: Double, probe: Double, correct: Boolean)
  private def dms: Study =
    val sampling = frame(210)
    val rows = seeds.zipWithIndex.flatMap { (seed, run) =>
      val random = new Mulberry(seed)
      val cells = shuffle(for load <- Vector("low", "high"); stim <- Vector("face", "scene"); _ <- 0 until 6 yield (load, stim), random)
      var time = 8.0
      cells.zipWithIndex.map { case ((load, stim), index) =>
        val duration = Vector(4.0, 6.0, 8.0)((random.next() * 3).toInt)
        val rt = math.rint((0.55 + random.next() * 0.8 + (if load == "high" then 0.18 else 0.0)) * 1000.0) / 1000.0
        random.next() // match field, retained only to preserve the generator stream
        val row = DmsRow(run, index + 1, load, stim, time, time + 2.0, duration, time + 2.0 + duration, run == 1 || !Set(4, 12, 19).contains(index))
        time += 2.0 + duration + rt + 4.0 + random.next() * 4.0
        row
      }
    }
    def phased(tag: String, phaseName: String, selected: Vector[DmsRow], onset: DmsRow => Double, duration: DmsRow => Double, factors: Vector[(String, DmsRow => String)]): ConvolvedTerm =
      val phase = PhaseId.unsafe(phaseName)
      val provenance = selected.map { row =>
        EventRowProvenance(TrialId.unsafe(row.trial.toString), Some(phase), row.trial - 1, row.run, Seconds(onset(row)), Seconds(duration(row)))
      }
      term(sampling, tag, factors.map((name, value) => name -> selected.map(value)), selected.map(onset), selected.map(duration), selected.map(_.run), Some(phase), provenance)
    val correct = rows.filter(_.correct)
    val errors = rows.filterNot(_.correct)
    val terms = Vector(
      phased("sample", "encode", rows, _.sample, _ => 2.0, Vector("load" -> ((row: DmsRow) => row.load), "stim" -> ((row: DmsRow) => row.stim))),
      phased("delay", "maintain", rows, _.delay, _.delayDuration, Vector("load" -> ((row: DmsRow) => row.load))),
      phased("probe", "retrieve", correct, _.probe, _ => 0.0, Vector("load" -> ((row: DmsRow) => row.load))),
      phased("errors", "retrieve", errors, _.probe, _ => 0.0, Vector.empty)
    )
    val parents = ParentTrialOnsets.validated(rows.map(row => ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(row.run + 1), TrialId.unsafe(row.trial.toString)), Seconds(row.sample)))).toOption.get
    Study("dms", model(sampling, terms), Some(parents))

  private def stop: Study =
    val sampling = frame(200)
    val rows = seeds.zipWithIndex.flatMap { (seed, run) =>
      val random = new Mulberry(seed); var time = 4.0; val out = Vector.newBuilder[(Double, String, Int)]
      while time < 380.0 do
        val value = random.next(); val outcome = if value < .7 then "go" else if value < .85 then "stop_success" else "stop_fail"
        if outcome != "stop_success" then
          val _ = random.next()
        if outcome != "go" then
          val _ = random.next()
        out += ((math.rint(time * 100.0) / 100.0, outcome, run)); time += 1.5 + random.next() * 3.0
      out.result()
    }
    Study("stop", model(sampling, Vector(term(sampling, "trials", Vector("outcome" -> rows.map(_._2)), rows.map(_._1), Vector.fill(rows.size)(0.0), rows.map(_._3)))), None)

  private def dense: Study =
    val sampling = frame(240)
    val rows = seeds.zipWithIndex.flatMap { (seed, run) =>
      val random = new Mulberry(seed)
      val cells = shuffle(for category <- Vector("faces", "bodies", "places", "objects", "words", "scrambled"); task <- Vector("attend", "ignore"); _ <- 0 until 11 yield (category, task), random)
      var time = 4.0
      cells.map { (category, task) =>
        val row = (math.rint(time * 100.0) / 100.0, category, task, run)
        random.next(); time += 2.2 + random.next() * 1.2; row
      }
    }
    Study("dense", model(sampling, Vector(term(sampling, "stim", Vector("category" -> rows.map(_._2), "task" -> rows.map(_._3)), rows.map(_._1), Vector.fill(rows.size)(1.0), rows.map(_._4)))), None)

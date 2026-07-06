package scalafim.fmri.design.contrast

import scalafim.fmri.design.event.{CategoricalEvent, EventTerm}

final case class Cell(vars: Vector[String], levels: Vector[String]):
  require(vars.length == levels.length, "vars/levels length mismatch")

  def get(varName: String): Option[String] =
    val i = vars.indexOf(varName)
    if i < 0 then None else Some(levels(i))

  def apply(varName: String): String =
    get(varName).getOrElse(throw new NoSuchElementException(s"Unknown variable: '$varName'"))

final case class TermCell(levels: Vector[String], count: Int):
  require(count >= 0, "count must be non-negative")

final case class TermCells(vars: Vector[String], rows: Vector[TermCell]):
  def shortNames: Vector[String] =
    rows.map(_.levels.mkString(":"))

object TermCells:

  def from(term: EventTerm, dropEmpty: Boolean = true): TermCells =
    val cats = term.events.collect { case c: CategoricalEvent => c }
    val n = term.onsets.length

    if cats.isEmpty then
      val v = term.events.headOption.map(_.varName).getOrElse("all_events")
      val row = TermCell(levels = Vector(v), count = n)
      if dropEmpty && row.count == 0 then TermCells(Vector(v), Vector.empty)
      else TermCells(Vector(v), Vector(row))
    else
      val vars = cats.map(_.varName)
      val levelsPerVar = cats.map(_.levels)
      val sizes = levelsPerVar.map(_.length)
      require(sizes.forall(_ >= 0), "invalid levels")

      // Full grid in R expand.grid order: first factor varies fastest.
      val grid = expandGrid(levelsPerVar)

      val counts = Array.fill(grid.length)(0)
      var i = 0
      while i < n do
        var idx = 0
        var mult = 1
        var ok = true
        var j = 0
        while j < cats.length do
          val code = cats(j).codes(i)
          val L = sizes(j)
          if code < 0 || code >= L then ok = false
          else idx += code * mult
          mult *= L
          j += 1
        if ok then counts(idx) += 1
        i += 1

      val rows =
        if dropEmpty then
          grid.indices.iterator
            .filter(r => counts(r) > 0)
            .map(r => TermCell(grid(r), counts(r)))
            .toVector
        else
          grid.indices.iterator.map(r => TermCell(grid(r), counts(r))).toVector

      TermCells(vars, rows)

  private def expandGrid(levelLists: Vector[Vector[String]]): Vector[Vector[String]] =
    levelLists.foldLeft(Vector(Vector.empty[String])) { (acc, levels) =>
      val out = Vector.newBuilder[Vector[String]]
      levels.foreach(lvl => acc.foreach(row => out += (row :+ lvl)))
      out.result()
    }

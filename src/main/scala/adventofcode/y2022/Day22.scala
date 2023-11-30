package adventofcode.y2022

import adventofcode.y2021.Grid
import adventofcode.y2021.Store
import adventofcode.y2021.Vect2D
import adventofcode.y2021.given

import javax.print.attribute.standard.PresentationDirection

@main
def Day22 = withResource("day22a.txt") {

  val mapLines :+ _ :+ directionsLine = lines()

  enum Action:
    case Walk(amount: Int)
    case Turn(amount: Int)

/*
  val surroundings: Seq[Vect2D[Int]] =
    Seq(1, -1).flatMap(i => Seq(Vect2D(i, 0), Vect2D(0, i))) // right, down, left, up
*/

  val actions = Seq.unfold((directionsLine, true)) {
    case ("", _) => None
    case (line, true) =>
      val (AsInt(num), rest) = line.span(_.isDigit)
      Some(Action.Walk(num) -> (rest, false))
    case (line, false) =>
      val (dir, rest) = line.splitAt(1)
      Some(Action.Turn(if (dir == "L") -1 else 1) -> (rest, true))
  }

  println(actions)

  val grid = mapLines.map(_.map(_.toString))

  val allPoints = grid.zipWithIndex.flatMap((line, y) => line.zipWithIndex.filterNot(_._1.isBlank).map((_, x) => Vect(x, y)))
  val xmap = allPoints.groupMap(_.x)(_.y).view.mapValues(seq => seq.min -> seq.max).toMap
  val ymap = allPoints.groupMap(_.y)(_.x).view.mapValues(seq => seq.min -> seq.max).toMap
  val limits = allPoints.map { v =>
    val (xmin, xmax) = xmap(v.x)
    val (ymin, ymax) = ymap(v.y)
    v -> Dir.values.zip(Seq(Vect(0, xmax), Vect(ymax, 0), Vect(0, xmin), Vect(ymin, 0))).toMap
  }.toMap

  def turn(dir: Dir, amount: Int): Dir = Dir.fromOrdinal((dir.ordinal + amount + 4) % 4)

  def move(here: Vect, dir: Dir): Vect = {
    val candidate = here + dir.vec


    here
  }


/*
  // unsafe: possible to collect non-existing points
  val store = Grid[String](v => grid(v.y)(v.x), cursor = Vect(0, 0))

  val walk: Grid[Map[Dir, Vect]] = store.extend { (s: Grid[String]) =>
    Dir.values.toSeq
      .map { dir =>
        val next = s.cursor + dir.vec
        store.peek(next)
        dir -> next
      }
      .toMap
  }
*/

    // mapLines.zipWithIndex.map((line, y) => line.zipWithIndex.map)

}
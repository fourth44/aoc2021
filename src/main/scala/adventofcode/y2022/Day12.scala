package adventofcode.y2022

import adventofcode.y2021.{ aStar, plusSurroundings, surroundings, manhattan }
import adventofcode.y2021.Store
import adventofcode.y2021.Grid
import adventofcode.y2021.Vect2D
import adventofcode.y2021.given

@main
def Day12 = withResource("day12a.txt") {

  val grid0: IndexedSeq[IndexedSeq[Char]] = lines().toIndexedSeq.map(_.toSeq)
  val allPoints: Seq[Vect2D[Int]] =
    grid0.indices.flatMap(y => grid0.head.indices.map(x => Vect2D(x, y)))

  val size = Vect(grid0.head.size - 1, grid0.size - 1)

  val grid: Grid[Char] = Store[Vect, Char](v => grid0(v.y)(v.x), Vect(0, 0)) // mostly for peeking logic and finding start and end

  val start = allPoints.find(grid.peek(_) == 'S').get
  val end = allPoints.find(grid.peek(_) == 'E').get

  val valueGrid = Store[Vect, Char](grid.peek(_) match {
    case 'S' => 'a'
    case 'E' => 'z'
    case other => other
  }, Vect(0, 0))

  def stepSize(from: Vect, to: Vect): Int = valueGrid.peek(to) - valueGrid.peek(from)

  def surroundingsStep(from: Vect, stepFilter: Int => Boolean) = plusSurroundings(from, size).filter(s => stepFilter(stepSize(from, s)))

  val part1 = aStar(start, _ == end, c => surroundingsStep(c, _ <= 1), cost = (_, _) => 1, manhattan(_, end))
  println(part1)

  val part2 = aStar(end, valueGrid.peek(_) == 'a', surroundingsStep(_, _ >= -1), cost = (_, _) => 1, valueGrid.peek(_) - 'a')
  println(part2)

}
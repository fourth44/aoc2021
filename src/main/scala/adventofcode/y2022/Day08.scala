package adventofcode.y2022

import adventofcode.y2021.Store
import adventofcode.y2021.Grid
import adventofcode.y2021.Vect2D
import adventofcode.y2021.given

@main
def Day08 = withResource("day08a.txt") {

  val grid: Seq[Seq[Int]] = lines().map(_.map(_.toString.toInt))
  val allPoints: Seq[Vect2D[Int]] = grid.indices.flatMap(y => grid.head.indices.map(x => Vect2D(x, y)))
  val depthStore = Store[Vect2D[Int], Int](v => grid(v.y)(v.x), cursor = Vect2D(0, 0))
  val (width, height) = (grid.head.size, grid.size)

  extension(vec: Vect2D[Int]) {
    def left = (0 until vec.x).map(Vect2D(_, vec.y)).reverse
    def right = (vec.x + 1 until width).map(Vect2D(_, vec.y))
    def up = (0 until vec.y).map(Vect2D(vec.x, _)).reverse
    def down = (vec.y + 1 until height).map(Vect2D(vec.x, _))
  }

  val outlookGrid: Grid[(Int, Seq[Seq[Int]])] = // i.e. own height + the heights in all viewing directions
    depthStore.extend { store =>
      val outlooks = Seq[Vect2D[Int] => Seq[Vect2D[Int]]](_.left, _.right, _.up, _.down).map(store.experiment)
      store.extract -> outlooks
    }

  val isVisibleGrid: Grid[Boolean] = outlookGrid.map { case (here, outlook) => outlook.exists(_.forall(_ < here))}

  val numVisible = allPoints.map(isVisibleGrid.peek).count(identity)
  println(numVisible)

  val scenicScoreGrid: Grid[Int] = outlookGrid.map {
    case (here, outlooks) =>
      outlooks.map { outlook =>
        val (smaller, rest) = outlook.span(_ < here)
        smaller.size + rest.headOption.size
      }.product
  }

  val maxScore = allPoints.map(scenicScoreGrid.peek).max
  println(maxScore)
}

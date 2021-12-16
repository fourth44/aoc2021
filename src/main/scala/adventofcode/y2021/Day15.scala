package adventofcode.y2021

import scala.math.Integral.Implicits.given

val surroundings: Seq[Vect2D[Int]] =
  Seq(-1, 1).flatMap(i => Seq(Vect2D(i, 0), Vect2D(0, i))) // up, down, left, right

def plusSurroundings(c: Vect2D[Int], end: Vect2D[Int]) = surroundings.map(_ + c).filter { case Vect2D(x, y) =>
  x >= 0 && x <= end.x && y >= 0 && y <= end.y // keep only surroundings that exist
}

// A* algorithm for grids
def aStar(start: Vect2D[Int], end: Vect2D[Int], cost: Vect2D[Int] => Int) =
  val zero = Map(start -> 0)
  def h(x: Vect2D[Int]): Int = // manhattan distance to end
    val dist = end + (x * -1)
    dist.x + dist.y
  (zero, zero).unfold { case (fringe, gmap) => // fringe has f() as value (g() + h()), gmap has g()
    val select: Vect2D[Int] = fringe.minBy(_._2)._1 // fringe has F score
    val gCurr = gmap(select)
    if (select == end) {
      Right(gCurr)
    } else {
      val newFringeG = plusSurroundings(select, end)
        .map { v => (v, gCurr + cost(v)) }
        .filter { case (v, g1) => gmap.get(v).forall(g0 => g1 < g0) }
        .toMap
      val newFringeF = newFringeG.map { (v, g) => (v, g + h(v)) } // add heuristic to g to obtain f
      Left((fringe.filter(_._1 != select) ++ newFringeF, gmap ++ newFringeG))
    }
  }


@main def  Day15: Unit = withResource("day15a.txt"){

  val grid0: IndexedSeq[IndexedSeq[Int]] = lines().toIndexedSeq.map(_.map(_.toString.toInt))

  val end = Vect2D(grid0.head.size - 1, grid0.size - 1)

  val resultA = aStar(Vect2D(0, 0), end, s => grid0(s.y)(s.x))

  println(resultA)

  val end2 = Vect2D(grid0.head.size * 5 - 1, grid0.size * 5 - 1)

  // index function based on input grid: there is no larger grid being build
  val resultB = aStar(Vect2D(0, 0), end2, { s =>
    val (dy, my) = s.y /% (end.y + 1)
    val (dx, mx) = s.x /% (end.x + 1)
    (grid0(my)(mx) - 1 + dx + dy) % 9 + 1
  })

  println(resultB)
}

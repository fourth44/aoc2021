package adventofcode.y2021

import scala.math.Integral.Implicits.given

val surroundings: Seq[Vect2D[Int]] =
  Seq(-1, 1).flatMap(i => Seq(Vect2D(i, 0), Vect2D(0, i))) // up, down, left, right

def plusSurroundings(c: Vect2D[Int], boardSize: Vect2D[Int]): Seq[Vect2D[Int]] =
  surroundings.map(_ + c)
    .filter { case Vect2D(x, y) => x >= 0 && x <= boardSize.x && y >= 0 && y <= boardSize.y }

def manhattan(from: Vect2D[Int], to: Vect2D[Int]): Int = (to - from).seq.map(_.abs).sum

// A* algorithm for grids
// todo remember WHAT path was taken!!
def aStar[A](start: A,
          isEnd: A => Boolean,
          surroundings: A => Seq[A],
          cost: (A, A) => Int, // cost of going from one A to another A. (do we ever need the 'from'?)
          h: A => Int // heuristic which should be better or at least as good as optimal path (do we also need the 'from'?)
         ): (Int, Seq[A]) =
  val zero = Map(start -> (0, Seq[A]())) // todo keep path here?
  (zero, zero).unfold { case (fringe, gmap) => // fringe has f() as value (g() + h()), gmap has g()
    val select: A = fringe.minBy(_._2._1)._1 // fringe has F score
    val gCurr = gmap(select)
    if (isEnd(select)) {
      Right(gCurr)
    } else {
      val newFringeG = surroundings(select)
        .map { v => (v, (gCurr._1 + cost(select, v), gCurr._2 :+ v)) }
        .filter { case (v, (g1, _)) => gmap.get(v).forall(g0 => g1 < g0._1) }
        .toMap
      val newFringeF = newFringeG.map { case (v, (g, p)) => (v, (g + h(v), p)) } // add heuristic to g to obtain f
      Left((fringe.filter(_._1 != select) ++ newFringeF, gmap ++ newFringeG))
    }
  }


@main def Day15: Unit = withResource("day15a.txt"){

  val grid0: IndexedSeq[IndexedSeq[Int]] = lines().toIndexedSeq.map(_.map(_.toString.toInt))

  val end = Vect2D(grid0.head.size - 1, grid0.size - 1)

  val resultA = aStar(Vect2D(0, 0), _ == end, plusSurroundings(_, boardSize = end), cost = (_, s) => grid0(s.y)(s.x), manhattan(_, end))

  println(resultA)

  val end2 = Vect2D(grid0.head.size * 5 - 1, grid0.size * 5 - 1)

  // index function based on input grid: there is no larger grid being build
  val resultB = aStar(Vect2D(0, 0), _ == end2, plusSurroundings(_, boardSize = end2), { (_, s) =>
    val (dy, my) = s.y /% (end.y + 1)
    val (dx, mx) = s.x /% (end.x + 1)
    (grid0(my)(mx) - 1 + dx + dy) % 9 + 1
  }, manhattan(_, end))

  println(resultB)
}

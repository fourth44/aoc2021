package adventofcode.y2021

// comonadic store
case class Store[S,A](peek: S => A, cursor: S):
  def extract: A = peek(cursor)
  def extend[B](f: Store[S,A] => B): Store[S, B] = Store(s => f(Store(peek, s)), cursor)
  def map[B](f: A => B): Store[S,B] = extend(s => f(s.extract))
  def zip[B](other: Store[S, B]): Store[S, (A, B)] = Store(s => (peek(s), other.peek(s)), cursor)

@main def Day09 = withResource("day09a.txt") {

  val grid: IndexedSeq[IndexedSeq[Int]] = lines().toIndexedSeq.map(_.map(_.toString.toInt))

  // enumerate all grid point indices
  val allPoints: Seq[Vect2D[Int]] =
    grid.indices.flatMap(y => grid(0).indices.map(x => Vect2D(x, y)))

  val depthStore = Store[Vect2D[Int], Int](v => grid(v.y)(v.x), cursor = Vect2D(0, 0))

  val surroundings: Seq[Vect2D[Int]] =
    Seq(-1, 1).flatMap(i => Seq(Vect2D(i, 0), Vect2D(0, i))) // up, down, left, right

  val surroundingStore: Store[Vect2D[Int], Seq[Vect2D[Int]]] =
    depthStore.extend { store =>
      surroundings
        .map(_ + store.cursor)
        .filter { case Vect2D(x, y) =>
          x >= 0 && x < grid(0).size && y >= 0 && y < grid.size // keep only surroundings that exist
        }
    }

  // local depth, and surroundings with their depths
  val surroundingsWithDepthsStore: Store[Vect2D[Int], (Int, Seq[(Vect2D[Int], Int)])] =
    depthStore zip surroundingStore.map(_.map(s => s -> depthStore.peek(s)))

  val lowPoints = allPoints.flatMap(v => surroundingsWithDepthsStore.peek(v) match {
    case (depth, surr) => Option.when(surr.forall(_._2 > depth))(v -> depth)
  })

  val resultA = lowPoints.map(_._2 + 1).sum

  println(resultA)

  val uphillStore = surroundingsWithDepthsStore.map { case (centerDepth, surr) =>
    surr.collect { case (s, d) if d > centerDepth && d < 9 => s }
  }

  val basins = lowPoints.map(_._1).map { lp =>
    Set(lp).unfold { start =>
      val all = start ++ start.flatMap(uphillStore.peek)
      val newPoints = all -- start
      Either.cond(newPoints.isEmpty, all, all) // done when empty, 'all' is both stop condition and next interation
    }
  }

  val topThree = basins.sortBy(_.size).reverse.take(3)
  val product = topThree.map(_.size).product

  // part II
  println(product)


}

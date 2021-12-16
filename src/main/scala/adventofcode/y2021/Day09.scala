package adventofcode.y2021

// functor typeclass: all things mappable
trait Functor[F[_]] {
  extension[A](a: F[A]) def fmap[B](f: A => B): F[B]
}

given Functor[Seq] with
  extension[A](a: Seq[A]) def fmap[B](f: A => B): Seq[B] = a.map(f)

// comonadic store
case class Store[S,A](peek: S => A, cursor: S):
  def extract: A = peek(cursor)
  def extend[B](f: Store[S,A] => B): Store[S, B] = Store(s => f(Store(peek, s)), cursor) // coFlatMap
  def map[B](f: A => B): Store[S,B] = extend(s => f(s.extract))
  def zip[B](other: Store[S, B]): Store[S, (A, B)] = Store(s => (peek(s), other.peek(s)), cursor)
  def experiment[F[_]: Functor](fn: S => F[S]): F[A] = fn(cursor).fmap(peek)

type Grid[A] = Store[Vect2D[Int], A]

@main def Day09 = withResource("day09a.txt") {

  val grid: IndexedSeq[IndexedSeq[Int]] = lines().toIndexedSeq.map(_.map(_.toString.toInt))

  // enumerate all grid point indices
  val allPoints: Seq[Vect2D[Int]] =
    grid.indices.flatMap(y => grid(0).indices.map(x => Vect2D(x, y)))

  val depthStore = Store[Vect2D[Int], Int](v => grid(v.y)(v.x), cursor = Vect2D(0, 0))

  val surroundings: Seq[Vect2D[Int]] =
    Seq(-1, 1).flatMap(i => Seq(Vect2D(i, 0), Vect2D(0, i))) // up, down, left, right

  def withSurroundings[A](store: Grid[A]): (A, Seq[A]) =
    store.extract -> store.experiment { c =>
      surroundings
        .map(_ + c)
        .filter { case Vect2D(x, y) =>
          x >= 0 && x < grid(0).size && y >= 0 && y < grid.size // keep only surroundings that exist
        }
    }

  val depthSurroundingsStore: Store[Vect2D[Int], (Int, Seq[Int])] =
    depthStore.extend(withSurroundings)

  val lowPointsStore: Store[Vect2D[Int], Option[Int]] =
    depthSurroundingsStore.map { (depth, surr) => Option.when(surr.forall(_ > depth))(depth) }

  val (lowPoints, lowPointsPositions) = allPoints.flatMap(p => lowPointsStore.peek(p).map(_ -> p)).unzip

  val resultA = lowPoints.map(_ + 1).sum

  println(resultA)

  val uphillStore: Store[Vect2D[Int], Seq[Vect2D[Int]]] =
    depthStore.zip(depthStore.extend(_.cursor)).extend(withSurroundings).map {
      case ((centerDepth, _), surr) =>
        surr.collect { case (d, s) if d > centerDepth && d < 9 => s }
    }

  val basins = lowPointsPositions.map { lp =>
    Set(lp).unfold { start => // repeatedly walk up until no more new points.
      val all = start ++ start.flatMap(uphillStore.peek)
      Either.cond((all -- start).isEmpty, all, all) // done when empty, 'all' is both stop condition and next iteration
    }
  }

  val topThree = basins.sortBy(_.size).reverse.take(3)
  val product = topThree.map(_.size).product

  // part II
  println(product)



}

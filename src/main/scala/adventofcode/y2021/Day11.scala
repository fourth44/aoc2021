package adventofcode.y2021

@main def Day11: Unit = withResource("day11a.txt") {

  type IntGrid = IndexedSeq[IndexedSeq[Int]]

  val grid0: IntGrid = lines().toIndexedSeq.map(_.map(_.toString.toInt))

  val allPoints: Seq[Vect2D[Int]] =
    grid0.indices.flatMap(y => grid0(0).indices.map(x => Vect2D(x, y)))

  val surroundings: Seq[Vect2D[Int]] = // all 8 neighbours
    (-1 to 1).flatMap(y => (-1 to 1).map(x => Vect2D(x, y))).filterNot(v => v.x == 0 && v.y == 0)

  def withSurroundings[A](store: Grid[A]): (A, Seq[A]) =
      store.extract -> store.experiment { c =>
        surroundings
          .map(_ + c)
          .filter { case Vect2D(x, y) =>
            x >= 0 && x < grid0(0).size && y >= 0 && y < grid0.size // keep only surroundings that exist
          }
      }

  // get all values from a store from all points (losing their positions)
  def execute[A](st: Grid[A]): Seq[A] = st.experiment(_ => allPoints)

  // from 2 dimensional collection to a Store (Grid) that uses that collection as lookup function
  def ints2store(gr: IntGrid): Grid[Int] = Store[Vect2D[Int], Int](v => gr(v.y)(v.x), cursor = Vect2D(0, 0))

  // a Store can be build of many layers of functions. snapshot replaces that with calculated values
  def snapshot[A](st: Grid[A]): Grid[A] =
    val indexed: Map[Vect2D[Int], A] = execute(st.extend(_.cursor).zip(st)).toMap
    Store(indexed, st.cursor)

  def print(grid: IntGrid) = grid.map(_.mkString).mkString("\n")

  val timetable = LazyList.iterate(ints2store(grid0) -> 0) { (gr1, acc) =>
    val gr2 = gr1.map(_ + 1)
    val gr5 = (gr2.map(_ -> false)).unfold { gr3 =>
      val gr4 = snapshot(gr3.extend(withSurroundings)).map {
        case ((i, _), surr) => (i + surr.count { case (j, didFire2) => !didFire2 && j >= 10 }, i >= 10)
      }
      val isDone = execute(gr4).count { (i, didFire) => !didFire && i >= 10 } == 0
      Either.cond(isDone, gr4, gr4)
    }
    val flashes = execute(gr5).count(_._2)
    val gr6 = gr5.map(_._1).map {
      case i if i >= 10 => 0
      case i => i
    }
    snapshot(gr6) -> flashes
  }

  val (fl100) = timetable.take(101).foldLeft(0)(_ + _._2) // including initial state so +1
  println(fl100)

  println(timetable.zipWithIndex.find(_._1._2 == grid0.size * grid0.head.size).map(_._2))

}

package adventofcode.y2021

@main def Day25: Unit = withResource("day25a.txt") {

  enum Cell:
    case Right, Down, Empty

  val grid: Seq[IndexedSeq[Cell]] = lines().map(_.map {
    case '>' => Cell.Right
    case 'v' => Cell.Down
    case '.' => Cell.Empty
  })
  val width = grid.head.size
  val height = grid.size
  val allPoints: Seq[Vect2D[Int]] = grid.indices.flatMap(y => grid(0).indices.map(x => Vect2D(x, y)))

  type Grid[A] = Store[Vect2D[Int], A]

  val store0 = Store[Vect2D[Int], Cell](v => grid(v.y)(v.x), Vect2D(0, 0))

  def horz(c: Vect2D[Int]): Seq[Vect2D[Int]] = Seq(c.copy(x = (c.x + width - 1) % width), c, c.copy(x = (c.x + 1) % width))
  def vert(c: Vect2D[Int]): Seq[Vect2D[Int]] = Seq(c.copy(y = (c.y + height - 1) % height), c, c.copy(y = (c.y + 1) % height))

  def stepH(g: Grid[Cell]): Grid[Cell] = g.extend(_.experiment(horz)).map {
    case Seq(_, Cell.Right, Cell.Empty) => Cell.Empty
    case Seq(Cell.Right, Cell.Empty, _) => Cell.Right
    case Seq(_, other, _) => other
  }
  def stepV(g: Grid[Cell]): Grid[Cell] = g.extend(_.experiment(vert)).map {
    case Seq(_, Cell.Down, Cell.Empty) => Cell.Empty
    case Seq(Cell.Down, Cell.Empty, _) => Cell.Down
    case Seq(_, other, _) => other
  }
  def execute[A](st: Grid[A]): Seq[A] = st.experiment(_ => allPoints)
  def snapshot[A](st: Grid[A]): Grid[A] =
    val indexed: Map[Vect2D[Int], A] = execute(st.extend(_.cursor).zip(st)).toMap
    Store(indexed, st.cursor)

  import scala.util.chaining.*

  val timeline = Iterator.iterate(store0)(s => s.pipe(stepH).pipe(stepV).pipe(snapshot))

/*
  def print(g: Grid[Cell]): String =
    (0 until height).map(y => (0 until width).map(x => g.peek(Vect2D(x, y)).match {
      case Cell.Right => ">"; case Cell.Down => "v"; case Cell.Empty => "."
    }).mkString).mkString("\n")
*/

  println(timeline.map(print).sliding(2).indexWhere(s => s.head == s(1)) + 1)

}

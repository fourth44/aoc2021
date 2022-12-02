package adventofcode.y2021


case class Fix[+F[+_]](value: F[Fix[F]])



@main def Day23: Unit = withResource("day23a.txt") {

  enum Cell:
    case Amphipod(kind: Char)
    case Open

  type Level = Map[Vect2D[Int], Cell.Amphipod]

  val map0 = lines().zipWithIndex.flatMap { case (line, y) =>
    line.zipWithIndex.flatMap {
      case ('.', x) => Some(Vect2D(x, y) -> Cell.Open)
      case (a, x) => Some(Vect2D(x, y) -> Cell.Amphipod(a))
      case _ => None
    }
  }.toMap

  val surroundings: Seq[Vect2D[Int]] = Seq(-1, 1).flatMap(i => Seq(Vect2D(i, 0), Vect2D(0, i))) // up, down, left, right
  def getSurroundings(v: Vect2D[Int]): Seq[Vect2D[Int]] = surroundings.map(s => v + s).filter(map0.keySet)

  val targetCols = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)
  val costs = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)

  case class State(map: Level, cost: Int)

  def getPods(map: Level, col: Int => Boolean = _ => true, row: Int => Boolean = _ => true): Level =
    map.collect { case (v, a: Cell.Amphipod) if col(v.x) && row(v.y) => (v, a) }


/*
  def moves(state: State) = {
    // find amphipods in above of the target columns: these must move first!
    val toMove = getPods(state.map, targetCols.values.toSeq.contains, _ == 1) match
      case map2 if map2.nonEmpty => map2 // subset of those right above room
      case _ => state.map

    val nextStates = for
        (v, a) <- toMove.toSeq
        np <- getSurroundings(v)
        if !state.map.keySet(np)
      yield
        State(state.map - v + (np -> a))
    // val nextStates = toMove.toSeq.flatMap { case (v, a) => getSurroundings(v).filter(v => !state.map.keySet(v)).map { np => map - v + (np -> a) }}

  }
*/

}

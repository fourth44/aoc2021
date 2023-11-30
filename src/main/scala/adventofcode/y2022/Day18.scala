package adventofcode.y2022

@main
def Day18 = withResource("day18a.txt") {

  val coordinates: Seq[Seq[Int]] = lines().map(_.split(",").map(_.toInt).toSeq)

  enum Axis:
    case X, Y, Z

  def minmax(in: Seq[Seq[Int]], axis: Axis): Range =
    val oneDim = in.map(_(axis.ordinal))
    oneDim.min - 1 to oneDim.max + 1

  val axisRange: Map[Axis, Range] = Axis.values.toSeq.map { (a: Axis) => a -> minmax(coordinates, a) }.toMap

  def xray(axis: Axis, points: Seq[Seq[Int]]): Int = { // count number of transitions between open <> closed
    val Seq(otherAxis1, otherAxis2) = Axis.values.toSeq.filterNot(_ == axis)
    val sliceMap: Map[(Int, Int), Seq[Seq[Int]]] = points.groupBy(seq => (seq(otherAxis1.ordinal), seq(otherAxis2.ordinal)))
    (for {
      a <- axisRange(otherAxis1)
      b <- axisRange(otherAxis2)
    } yield {
      sliceMap.get((a, b)).fold(0) { slice3D =>
        val slice1D = slice3D.map(_(axis.ordinal)).toSet
        (slice1D.min - 1 to slice1D.max + 1).map(slice1D).sliding(2).toSeq.count(_.distinct.size == 2)
      }
    }).sum
  }

  def surfaceCount(points: Seq[Seq[Int]]): Int = Axis.values.map(xray(_, points)).sum

  println(surfaceCount(coordinates)) // part 1

  val allPoints = for {
    x <- axisRange(Axis.X)
    y <- axisRange(Axis.Y)
    z <- axisRange(Axis.Z)
  } yield {
    Seq(x, y, z)
  }

  val empty = allPoints.toSet -- coordinates.toSet

  def adjacent(a: Seq[Int], b: Seq[Int]): Boolean =
    val zipped: Seq[(Int, Int)] = a zip b
    zipped.count(_ == _) == 2 && zipped.count((a,b) => (a - b).abs == 1) == 1

  val connectedGroups = empty.foldLeft(Set[Set[Seq[Int]]]()) { (acc, point) =>
    val (adjacentGroups, nonAdjacentGroups) = acc.partition { _.exists(adjacent(_, point))}
    nonAdjacentGroups union Set(adjacentGroups.flatten union Set(point))
  }

  val pockets = connectedGroups.toSeq.sortBy(_.size).init // discard largest pocket ('outside air')

  println(surfaceCount(coordinates ++ pockets.flatten)) // part2
}

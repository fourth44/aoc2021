package adventofcode.y2021

enum Axis:
  case x, y

case class Fold(axis: Axis, line: Int)

@main def Day13: Unit = withResource("day13a.txt") {

  val (points0, folds0) = lines().span(_.nonEmpty)

  val points = points0.map { case r"(\d+)${AsInt(x)},(\d+)${AsInt(y)}" => Vect2D(x, y) }
  val folds = folds0.filter(_.nonEmpty).map { case r"fold along ([xy])$axis=(\d+)${AsInt(line)}" => Fold(Axis.valueOf(axis), line)}

  def fold(point: Vect2D[Int], on: Fold): Vect2D[Int] =
    on.axis match
      case Axis.x if point.x > on.line => Vect2D(on.line - (point.x - on.line), point.y)
      case Axis.y if point.y > on.line => Vect2D(point.x, on.line - (point.y - on.line))
      case _ => point

  def step(points: Seq[Vect2D[Int]], on: Fold): Seq[Vect2D[Int]] = points.map(fold(_, on)).distinct

  println(step(points, folds.head).size)

  val resultPoints = folds.foldLeft(points)(step).toSet // all work done here basically

  val resultsGrid = (0 to resultPoints.map(_.y).max).map(y => (0 to resultPoints.map(_.x).max).map(x =>
    if resultPoints(Vect2D(x, y)) then " #" else " ."
  ).mkString).mkString("\n")

  println(resultsGrid)

}

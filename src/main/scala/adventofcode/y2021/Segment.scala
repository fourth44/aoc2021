package adventofcode.y2021

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps
import adventofcode.y2021.Vect2D

// can be used as true segment between both points (inclusive) or as infinite line through both points
case class Segment[A: Numeric](a: Vect2D[A], b: Vect2D[A])

extension [N: Numeric: CanDiv](s: Segment[N]) {
  def isHorzVert: Boolean = s.a.y == s.b.y || s.a.x == s.b.x
  def minX: N = s.a.x min s.b.x
  def maxX: N = s.a.x max s.b.x
  def minY: N = s.a.y min s.b.y
  def maxY: N = s.a.y max s.b.y

  // see https://www.topcoder.com/thrive/articles/Geometry%20Concepts%20part%202:%20%20Line%20Intersection%20and%20its%20Applications#LineLineIntersection
  // also works for vertical lines
  // find form Ax+By=C
  def A: N = s.b.y - s.a.y
  def B: N = s.a.x - s.b.x
  def C: N = A * s.a.x + B * s.a.y
  def det(s2: Segment[N]): N = s.A * s2.B - s2.A * s.B // 0 if parallel

  def onLineSegment(v: Vect2D[N]): Boolean =
    v.x >= minX && v.x <= maxX && v.y >= minY && v.y <= maxY && s.onLine(v)

  def onLine(v: Vect2D[N]): Boolean =  // doesn't take line segment into account, only whole implied line
    val s1 = Segment(s.a, v) // create 2 lines: segment start -> v and v -> segment end
    val s2 = Segment(v, s.b)
    s1.det(s2) == 0 // if both those lines are parallel, v must be on the line implied by s (but not per se on line segment s!)

  def intersectLine(s2: Segment[N]): Option[Vect2D[N]] =
    val D = s.det(s2)
    Option.unless(D == 0) {
      Vect2D[N](
        (s2.B * s.C - s.B * s2.C) / D,
        (s.A * s2.C - s2.A * s.C) / D
      )
    }

  def intersectSegment(s2: Segment[N]): Option[Vect2D[N]] =
    intersectLine(s2).filter { i =>
      s.onLineSegment(i) && s2.onLineSegment(i)
    }

  def overlap(s2: Segment[N]): Option[Segment[N]] =
    Option.when(s.det(s2) == 0) {
      (s.toSeq ++ s2.toSeq).filter { v =>
        Seq(s, s2).forall(_.onLineSegment(v))
      }.distinct.match {
        case Seq(a) => Some(Segment(a, a))
        case Seq(a, b) => Some(Segment(a, b))
        case other => None
      }
    }.flatten

  def toSeq: Seq[Vect2D[N]] = Seq(s.a, s.b)
}

extension (s: Segment[Int]) {
  // suboptimal: tries every point in the bounding box!
  def integralPoints: Seq[Vect2D[Int]] =
    (for {
      x <- s.minX to s.maxX
      y <- s.minY to s.maxY
      v = Vect2D(x, y)
      if s.onLineSegment(v)
    } yield {
      v
    }).toSeq
}

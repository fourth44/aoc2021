package adventofcode.y2021

import scala.math.Ordering.Implicits.*

// can be used as true segment between both points (inclusive) or as infinite line through both points
case class Segment[A: Numeric](a: Vect2D[A], b: Vect2D[A])

object Segment {
  // 'extension' to keep the case class clean
  import scala.math.Numeric.Implicits.*
  extension [N](s: Segment[N])(using n: Numeric[N]) {

    def map[M: Numeric](f: N => M): Segment[M] = Segment(s.a.map(f), s.b.map(f))

    def zero = n.zero

    def isHorzVert: Boolean = A == zero || B == zero

    def minBy(by: Vect2D[N] => N) = by(s.a) min by(s.b)
    def maxBy(by: Vect2D[N] => N) = by(s.a) max by(s.b)

    // see https://www.topcoder.com/thrive/articles/Geometry%20Concepts%20part%202:%20%20Line%20Intersection%20and%20its%20Applications#LineLineIntersection
    // also works for vertical lines
    // find form Ax+By=C
    def A: N = s.b.y - s.a.y  // delta Y
    def B: N = s.a.x - s.b.x  // - delta X
    def C: N = A * s.a.x + B * s.a.y
    def det(s2: Segment[N]): N = s.A * s2.B - s2.A * s.B // 0 if parallel

    def onLineSegment(v: Vect2D[N]): Boolean =
      v.x >= minBy(_.x) && v.x <= maxBy(_.x) && v.y >= minBy(_.y) && v.y <= maxBy(_.y) && s.onLine(v)

    def onLine(v: Vect2D[N]): Boolean =  // doesn't take line segment into account, only whole implied line
      val s1 = Segment(s.a, v) // create 2 lines: segment start -> v and v -> segment end
      val s2 = Segment(v, s.b)
      s1.det(s2) == zero // if both those lines are parallel, v must be on the line implied by s (but not per se on line segment s!)

    inline def intersectLine(s2: Segment[N]): Option[Vect2D[N]] = // None if parallel
      val div: (N, N) => N = scala.compiletime.summonFrom {
        case i: Integral[N] => i.quot
        case f: Fractional[N] => f.div
      }
      val D = s.det(s2)
      Option.unless(D == zero) {
        Vect2D[N](
          div(s2.B * s.C - s.B * s2.C, D),
          div(s.A * s2.C - s2.A * s.C, D)
        )
      }

    inline def intersectSegment(s2: Segment[N]): Option[Vect2D[N]] =
      intersectLine(s2).filter { i =>
        s.onLineSegment(i) && s2.onLineSegment(i)
      }

    def overlap(s2: Segment[N]): Option[Segment[N]] =
      Option.when(s.det(s2) == zero) {                              // if parallel
        (s.toSeq ++ s2.toSeq)                                       // from all the points of both lines
          .filter { v => Seq(s, s2).forall(_.onLineSegment(v)) }    // keep those which are on both lines
          .distinct
      }.flatMap {
        case Seq(a) => Some(Segment(a, a))                          // lines extend each other (1 point overlap)
        case Seq(a, b) => Some(Segment(a, b))                       // other cases of overlap
        case other => None
      }

    def toSeq: Seq[Vect2D[N]] = Seq(s.a, s.b)
    def swapAxis: Segment[N] = Segment(s.a.swap, s.b.swap)
  }

  extension [N](s: Segment[N])(using num: Integral[N]) {

    def integralPoints: Seq[Vect2D[N]] =
      import math.Integral.Implicits.infixIntegralOps
      val s0 = s.B match
        case 0 => s.swapAxis                // vertical: swap axis
        case _ => s
      for
        x <- scala.collection.immutable.NumericRange.inclusive(s0.minBy(_.x), s0.maxBy(_.x), num.one) // all points on x axis
        (y, mod) = (s0.C - s0.A*x) /% s0.B  // compute y by inverting Ax+By=C into y=(C-Ax)/B
        if mod == 0                         // only if y is also integral
        v = Vect2D(x, y)
      yield
        s.B match
          case 0 => v.swap                  // swap results back if vertical
          case _ => v
  }

}


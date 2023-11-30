package adventofcode.y2022

import adventofcode.y2021.Segment
import adventofcode.y2021.Vect2D

import scala.util.chaining.*
import adventofcode.y2021.manhattan

import scala.math.Integral.Implicits.infixIntegralOps
@main
def Day15 = withResource("day15a.txt") {

  // sensors with their closest beacon
  val signalsAndBeacons = lines()
    .map { case s"Sensor at x=${AsInt(sx)}, y=${AsInt(sy)}: closest beacon is at x=${AsInt(bx)}, y=${AsInt(by)}" => Vect(sx, sy) -> Vect(bx, by) }

  val signalsAndDistances = signalsAndBeacons.map((a, b) => a -> manhattan(a, b))

  val (signals, beacons) = signalsAndBeacons.unzip.pipe((a, b) => (a.toSet, b.toSet))
  val (minX, maxX) = (signals ++ beacons).map(_.x).toSeq.sorted.pipe(xs => xs.head -> xs.last)
  val maxDist = signalsAndDistances.map(_._2).max

  def isCovered(point: Vect2D[Int]): Boolean = signalsAndDistances.exists((signal, dist) => manhattan(signal, point) <= dist)


/*
  val part1 = ((minX - maxDist) to (maxX + maxDist)).map(Vect(_, 2000000)).count(p => isCovered(p) && !beacons(p))
  println(part1)
*/

  def slice(signal: Vect, dist: Int, row: Int): Option[Segment[Int]] = {
    val d = dist - (signal.y - row).abs
    Option.unless(d < 0) { Segment(Vect(signal.x - d, row), Vect(signal.x + d, row)) }
  }

  val range = 4000000

  println(4000000L*2978645L+3249288L)

/*
  // go through all rows, find covered segments for that row, merge all overlapping segments, 'missing' element should have two non-overlapping segments :((
  (0 to range).foreach { y =>
    val slices0 = signalsAndDistances.flatMap { (signal, distance) =>
      slice(signal, distance, y)
    }.sortBy(_.minBy(_.x)).distinct

    val groups = slices0.foldLeft(Seq[Seq[Segment[Int]]]()) { (acc, slice) =>
      // hoe weet je of slice er nou wel of niet ergens in is gestopt?
      val addHere: Int = acc.indexWhere(_.exists(_.overlap(slice).nonEmpty))
      if (addHere != -1) {
        acc.updated(addHere, acc(addHere) :+ slice)
      } else {
        acc :+ Seq(slice)
      }
    }
    val segments = groups.flatMap(group => Segment(Vect(group.map(_.minBy(_.x)).min, y), Vect(group.map(_.maxBy(_.x)).max, y)).overlap(Segment(Vect(0, y), Vect(range, y))))
    if (segments.size > 1) println(segments)
  }
*/

  val answer = Vect(2978645, 3249288)

/*
  signalsAndDistances.map { case (signal, distance) =>
    val dist2 = manhattan(signal, answer)
    println(s"signal: $signal distance: ${distance} distance to answer: ${dist2} diff: ${dist2 - distance}")
  }
*/

  def boundingbox[A](signal: Vect2D[A], dist: A)(using num: Integral[A]): Seq[Segment[A]] = {
    lazy val points = Seq(-(dist + num.one ), dist + num.one).flatMap(i => Seq(Vect(i, num.zero), Vect(num.zero, i))).map(_ + signal)
    (points :+ points.head).sliding(2).map { case Seq(a, b) => Segment(a, b) }.toSeq
  }

  // val box = boundingbox(Vect(3171811L,2935106L), 507347L)//.flatMap(_.integralPoints)
  // println(box.head.integralPoints.take(10))
/*
  val s0 = box.head

  (s0.minBy(_.x) to s0.maxBy(_.x)).take(20).map { x =>
    val (y, mod) = (s0.C - s0.A * x) /% s0.B // compute y by inverting Ax+By=C into y=(C-Ax)/B
    println(s"y: $y, mod: $mod")
  } // all points on x axis
*/


  // each diamond has line segments

  // but this is still slow: you have all diamond edges (not that many), but they still need to be truncated at 0..4000000
  // 140 'lines' of length 4000000, but we can use same trick as above, 140 is a LOT less than doing the trick 4000000 times
  // they are diagonals
  val segments: Seq[Segment[Long]] = signalsAndDistances.flatMap((a, b) => boundingbox(a.map(_.toLong), b.toLong))
  val empty = segments.flatMap(_.integralPoints).distinct.filter(_.seq.forall(p => p <= range.toLong && p >= 0L)).filter(l => !isCovered(l.map(_.toInt)))
  println(empty.size)
  println(empty)


/*
  val board = (-2 to 22).map { y =>
    (-1 to 22).map { x =>
      Vect(x, y) match {
        case s if signals.contains(s) => "S "
        case b if beacons.contains(b) => "B "
        case c if isCovered(c) => "# "
        case _ => ". "
      }
    }.mkString
  }.mkString("\n")
  println(board)
*/

/*
  val part2 = for(x <- 0 to 4000000; y <- 0 to 4000000) yield {
    !signals(point) && !beacons(point) &&
      signalsAndDistances.exists((signal, dist) => manhattan(signal, point) <= dist)
  }
*/
}
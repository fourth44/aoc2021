package adventofcode.y2021

import scala.Ordering.Implicits.seqOrdering

object Foo {

@main def Day19: Unit = withResource("day19a.txt") {
  type Point3D = Seq[Int] // of size 3

  val parsed: Seq[(Int, Seq[Point3D])] = List.unfold(lines()) { lines =>
    lines.span(_.nonEmpty) match
       case (r"--- scanner (\d+)${AsInt(scannerNum)} ---" +: nums, rest) =>
         Some((scannerNum, nums.map(_.split(',').toSeq.map(_.toInt))) -> rest.drop(1))
       case (Seq(), _) =>
         None
  }

  extension [A](seq: Seq[A]) def cycle(i: Int): Seq[A] = seq.drop(i % seq.size) ++ seq.take(i % seq.size)

  // cos(90), -sin(90)    0, -1   y=-x
  // sin(90), cos(90)     1, 0    x=y

  extension (xyz: Point3D)
    def rotate(axis: Int): Point3D =
      (xyz, axis) match {
        case (Seq(x, y, z), 0) => Seq(x, z, -y)
        case (Seq(x, y, z), 1) => Seq(z, y, -x)
        case (Seq(x, y, z), 2) => Seq(y, -x, z)
      }
/*

      val (a, (b, c)) = xyz.splitAt(axis).match { case (a, b) => a -> b.splitAt(axis + 1 - a.size)}
      (a ++ c).match { case Seq(one, two) => Seq(-two, one).patch(axis, b, 0) }

*/
/*
      xyz.cycle(axis).match {
        case Seq(x, y, z) => Seq(x, -z, y) // lol don't even know whether this rotates left or right
      }.cycle(xyz.size - axis)
*/
    infix def plus(other: Point3D): Point3D = xyz.zip(other).map(_ + _)
    infix def mult(i: Int): Point3D = xyz.map(_ * i)

  def all4Rotations(point: Point3D, axis: Int): Seq[Seq[Int]] = Seq.iterate(point, 4)(_.rotate(axis))

  def all24Rotations(xyz: Point3D): Seq[Seq[Int]] =
    val fud = all4Rotations(xyz, 1).cycle(2).tail.cycle(1)  // forward, upward, downward (but not flipped backwards)
    val rolls = all4Rotations(xyz, 0).tail                  // forward but rolled right, upside down and rolled left
    (fud ++ rolls).flatMap(p => all4Rotations(p, 2))        // all of the above towards front, right, back and left

  def dist(seq1: Point3D, seq2: Point3D): Point3D = seq1.zip(seq2).map((a, b) => math.abs(b - a))

  // for reach scanner, all beacon-beacon distances as keys, with for each distance all points that have that distance
  val beaconDistances: Seq[(Int, Map[Set[Int], Seq[Seq[Int]]])] =
    parsed.map { case (scanner, beacons) =>
      scanner -> beacons.combinations(2).map { case Seq(one, two) => (dist(one, two).toSet, Set(one, two)) }.toSeq
        .groupMap(_._1)(_._2).view.mapValues(x => x.flatten).toMap
    }

  val overlaps = beaconDistances.combinations(2).toSeq.flatMap { case Seq((s1, bd1), (s2, bd2)) =>
    val intersections = bd1.intersect(bd2)
    if (intersections.values.toSeq.unzip.match { case (a, b) => a.flatten.size / 2 >= 66 && b.flatten.size / 2 >= 66 }) {
      Seq((s1, (s2, intersections)), (s2, (s1, intersections.view.mapValues(_.swap).toMap))) // both directions
    } else {
      None
    }
  }.groupMap(_._1)(_._2).view.mapValues(x => x.toMap).toMap

  println("all beaons:")
  println(parsed.flatMap(_._2).size)

  val q = overlaps.values.toSeq.flatMap(_.values.toSeq).map(_.size)

  // we can throw away distances now? we do need that grouping still
  // each List has two points of unknown rotation
  // try all rotations on two points of s1, see if it matches one in s2
  //         distance         pairs points s1     pairs points s2
  val r = overlaps(0)(1)(Set(38, 28, 29))

  val r2 = overlaps(0)(1).values.flatMap(_._1).toSeq.distinct.sorted
  val r3 = overlaps(0)(1).values.flatMap(_._2).toSeq.distinct

  val r4 = r3.map(all24Rotations).transpose.map(_.sorted)
  val r5 = r4.map{ twelve =>
    val diff = twelve.head plus (r2.head mult -1)
    twelve.map(_ plus diff)
  }
  println(r5.exists(_.contains(r2)))

/*
  // try one distance here8
  val (pointsS1, pointsS2) = r.match { case (f0, t0) => f0.sorted.distinct -> t0.distinct }

  val allPointsS2ByRotation: Map[Int, Seq[Seq[Int]]] = pointsS2.flatMap(all24Rotations(_).zipWithIndex).groupMap(_._2)(_._1.sorted)
  // find rotation that fits most pairs. still need to translate
  val foo = allPointsS2ByRotation.map { case (rotNr, points) =>
    // println(pointsS1.size == pointsS2.size)
    val diff = pointsS1.head plus (points.head mult -1)
//     val y = points.map(_ plus diff)
    val z = pointsS1.zip(points).map((b, a) => a plus (b mult -1) plus diff)
    // require((points.head plus diff) == pointsS1.head)
    //println(points.map(_ plus diff).map(_.toList))
    if (points.map(_ plus diff).toSet == pointsS1.toSet) {
      println(s"hoera ${rotNr}")
    }
    z

    // val distances = pointsS1.flatMap(pointS1 => points.map(pointS2 => dist(pointS1, pointS2))).groupBy(identity).view.mapValues(_.size).toMap

  }

  println(pointsS1.size)
  println(pointsS2.size)
  println(allPointsS2ByRotation.map(_._2.size))

  println(Seq(1,2,3) plus (Seq(1,2,3) mult -1))

  (all24Rotations(Seq(8, 0, 7))).foreach(println)
*/

  // val (a, (b, c))
  // val axis = 1
  // val foobar = Seq(8, 0, 7).splitAt(axis).match { case (a, b) => a -> b.splitAt(axis + 1 - a.size)}
  // println(foobar)
  // (a ++ c).match { case Seq(one, two) => Seq(-two, one).patch(axis, b, 0) }


  // all24Rotations(Seq(-485,-357,347)).foreach(println) // axis 1: turns rightwards?


/*
    pairsS2.map { pair =>
    val allRotations = pair.flatMap(all24Rotations(_).zipWithIndex).toSeq.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap
    // for each rotation find
  }
*/

}
}
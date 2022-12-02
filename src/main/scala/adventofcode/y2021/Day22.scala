package adventofcode.y2021

object Foo22 {
  @main def Day22: Unit = withResource("day22a.txt") {

    case class Point3(x: Int, y: Int, z: Int)

    import Parser.*

    def intersp[A](p: Parser[A], i: String, n: Int): Parser[List[A]] = (p ** (string(i) *> p).listOfN(n - 1)).map(_ :: _)

    val num2 = intersp(num, "..", 2)

    def range(axis: String) = string(s"$axis=") *> num2

    enum OnOff:
      case On, Off

    case class Interval(from: Int, to: Int) {
      def size: Int = to - from

      def contains(i: Int): Boolean = i >= from && i < to

      def intersect(other: Interval): Seq[Interval] = {
        Seq(this, other).sortBy(_.from).match {
          case Seq(one, two) if one.contains(two.from) && !one.contains(two.to) =>
            Seq(Interval(one.from, two.from), Interval(two.from, one.to), Interval(one.to, two.to))
          case Seq(one, two) if one.contains(two.from) && one.contains(two.to) =>
            Seq(Interval(one.from, two.from), two, Interval(two.to, one.to))
          case Seq(one, two) if one.to <= two.from => Seq(one, two)
          }.filterNot(i => i.from == i.to)
      }
    }

    case class Cuboid(x: Interval, y: Interval, z: Interval):
      def size: Int = x.size * y.size * z.size

      def contains(a: Int, b: Int, c: Int) = x.contains(a) && y.contains(b) && z.contains(c)

      def intersect(other: Cuboid): Seq[Cuboid] = {
        val (xs, ys, zs) = (x.intersect(other.x), y.intersect(other.y), z.intersect(other.z))
        val r = for
          ix <- xs
          iy <- ys
          iz <- zs
          if this.contains(ix.from, iy.from, iz.from) || other.contains(ix.from, iy.from, iz.from)
        yield
          Cuboid(ix, iy, iz)
        r
      }

    val onoff: Parser[(OnOff, Cuboid)] = ((string("on") | string("off")) ** (whitespace *> range("x")) ** (string(",") *> range("y")) ** (string(",") *> range("z"))).map {
      case (((onoff, List(x1, x2)), List(y1, y2)), List(z1, z2)) =>
        OnOff.valueOf(onoff.capitalize) -> Cuboid(Interval(x1, x2 + 1), Interval(y1, y2 + 1), Interval(z1, z2 + 1))
    }

    val parsed = lines().map(onoff.run(_).toOption.get)

    val hundred = parsed.filter { case (_, Cuboid(x, y, z)) => Seq(x.from, y.from, z.from).forall(_ >= -50) && Seq(x.to, y.to, z.to).forall(_ <= 50) }


    def enclosing(cuboids: Seq[Cuboid]) = Cuboid(
      Interval(cuboids.map(_.x.from).min, cuboids.map(_.x.to).max),
      Interval(cuboids.map(_.y.from).min, cuboids.map(_.y.to).max),
      Interval(cuboids.map(_.z.from).min, cuboids.map(_.z.to).max)
    )


    def intersectAll(cuboids: Seq[Cuboid]) =
      cuboids.tail.zipWithIndex.foldLeft(Seq(cuboids.head)) { case (acc, (cub, i)) =>
        println(s"$i, ${acc.size}")
        val r = acc.flatMap(_.intersect(cub))
        r.distinct // distinct zou niet nodig moeten zijn !?
      }

    val states = for { x <- -50 to 50; y <- -50 to 50; z <- -50 to 50 } yield {
      parsed.reverse.collectFirst { case (state, c) if c.contains(x, y, z) => state }.getOrElse(OnOff.Off)
    }
    println("---")
    println(states.count(_ == OnOff.On))
    println("---")

/*
    val xs = parsed.map(_._2).flatMap(c => Seq(c.x.from, c.x.to))
    val ys = parsed.map(_._2).flatMap(c => Seq(c.y.from, c.y.to))
    val zs = parsed.map(_._2).flatMap(c => Seq(c.z.from, c.z.to))

    println(s"${xs.size}, ${ys.size}, ${zs.size}")

    val points = for {x <- xs; y <- ys; z <- zs } yield { Point3(x, y, z)}
*/

    val intersections = intersectAll(hundred.map(_._2)).distinct
    val cuboidStates = intersections.map { p =>
      p -> parsed.findLast { case (state, c) => c.contains(p.x.from, p.y.from, p.z.from) }.fold(OnOff.Off)(_._1)
    }
    val areOn = cuboidStates.collect { case (c, OnOff.On) => c }
    val resultA = areOn.map(_.size).sum

    println(areOn.size)
    println(resultA)
    // 1216732346 te hoog

    // test: moet zijn: 590.784, ik gok: 1266263697. hoeveel cuboids aan denk ik: 271.564

    // probeer een soort R-tree?
    // case class Tree[A](value: A, children: Seq[Tree[A]])



  }
}
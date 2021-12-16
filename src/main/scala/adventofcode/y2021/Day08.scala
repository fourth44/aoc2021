package adventofcode.y2021

enum Wire:
  case a,b,c,d,e,f,g

enum Pos:
  case u,m,b,ul,ur,bl,br

enum Digits(val pos: Set[Pos], val num: Int):
  case one extends Digits(Set(Pos.ur, Pos.br), 1)
  case two extends Digits(Set(Pos.u, Pos.ur, Pos.m, Pos.bl, Pos.b), 2)
  case three extends Digits(Set(Pos.u, Pos.ur, Pos.m, Pos.br, Pos.b), 3)
  case four extends Digits(Set(Pos.ul, Pos.ur, Pos.m, Pos.br), 4)
  case five extends Digits(Set(Pos.u, Pos.ul, Pos.m, Pos.br, Pos.b), 5)
  case six extends Digits(Set(Pos.u, Pos.ul, Pos.m, Pos.bl, Pos.br, Pos.b), 6)
  case seven extends Digits(Set(Pos.u, Pos.ur, Pos.br), 7)
  case eight extends Digits(Pos.values.toSet, 8)
  case nine extends Digits(Set(Pos.u, Pos.ul, Pos.ur, Pos.m, Pos.br, Pos.b), 9)
  case zero extends Digits(Set(Pos.u, Pos.ul, Pos.ur, Pos.bl, Pos.br, Pos.b), 0)
  def size = pos.size


@main def Day08 = withResource("day08a.txt"){

  object wires:
    def unapply(s: String): Option[Seq[Set[Wire]]] = Some(s.split(' ').filter(_.nonEmpty).map(_.map(c => Wire.valueOf(c.toString)).toSet).toSeq)

  val parsed = lines().map { line =>
    val Seq(wires(signals), wires(outputs)) = line.split('|').toSeq.map(_.trim)
    (signals, outputs)
  }

  val resultA = {
    val histo = parsed.flatMap(_._2).map(_.size).groupBy(identity).view.mapValues(_.size).toMap
    histo.getOrElse(2, 0) + histo.getOrElse(4, 0) + histo.getOrElse(3, 0) + histo.getOrElse(7, 0)
  }
  println(resultA)

  val resultB = parsed.map { (signals, outputs) =>
    val allSignals = (signals ++ outputs).distinct.ensuring(_.sizeIs == 10) // all digits must be on all lines for this to work.
    val signalsBySize = allSignals.groupBy(_.size)

    val Seq(one) = signalsBySize(2)
    val Seq(four) = signalsBySize(4)
    val Seq(seven) = signalsBySize(3)
    val u_m_b = signalsBySize(5).foldLeft(Wire.values.toSet)(_ intersect _)
    val u_ul_br_b = signalsBySize(6).foldLeft(Wire.values.toSet)(_ intersect _)
    val u_b = u_m_b intersect u_ul_br_b
    val ul_m = four -- one
    val u = seven -- one
    val b = u_b -- u
    val m = u_m_b -- u_b
    val ul = ul_m -- m
    val br = u_ul_br_b -- u_b -- ul
    val ur = one -- br
    val bl = Wire.values.toSet -- Set(u,b,m,ul,br,ur).flatten
    val index = Map(u.head -> Pos.u, ul.head -> Pos.ul, ur.head -> Pos.ur, m.head -> Pos.m, bl.head -> Pos.bl, br.head -> Pos.br, b.head -> Pos.b)

    outputs.map(output => Digits.values.find(_.pos == output.map(index)).head.num).mkString.toInt
  }.sum

  println(resultB)

}

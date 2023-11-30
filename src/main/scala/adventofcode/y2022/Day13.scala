package adventofcode.y2022

import adventofcode.y2021.Parser
import adventofcode.y2021.Parser.*

@main
def Day13 = withResource("day13a.txt") {

  enum Packet:
    case Num(i: Int)
    case List(elems: Seq[Packet])

  lazy val packetParser: Parser[Packet] = num.map(Packet.Num(_)) | bracketed(csv(packetParser)).map(Packet.List(_))

  val pairs = lines().grouped(3).map(_.take(2).map(packetParser.run(_).toOption.get)).toSeq

  lazy val packetOrd: Ordering[Packet] = (left: Packet, right: Packet) =>
    (left, right) match
      case (Packet.Num(a), Packet.Num(b)) => a compareTo b
      case (Packet.List(elems1), Packet.List(elems2)) => Ordering.Implicits.seqOrdering(packetOrd).compare(elems1, elems2)
      case (num@Packet.Num(_), list@Packet.List(_)) => packetOrd.compare(Packet.List(Seq(num)), list)
      case (list@Packet.List(_), num@Packet.Num(_)) => packetOrd.compare(list, Packet.List(Seq(num)))

  val part1 = pairs.zipWithIndex
    .collect { case (Seq(left, right), i0) if packetOrd.compare(left, right) < 0 => i0 + 1 }
    .sum

  println(part1)

  val dividers: Seq[Packet] = Seq("[[2]]", "[[6]]").map(packetParser.run(_).toOption.get)
  val allPacketsSorted: Seq[Packet] = (pairs.flatten ++ dividers).sorted(packetOrd)
  val part2 = dividers.map(allPacketsSorted.indexOf).map(_ + 1).product
  println(part2)
}

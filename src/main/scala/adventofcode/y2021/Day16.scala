package adventofcode.y2021

@main def Day16: Unit = withResource("day16a.txt") {

  // Model
  case class Packet(header: Header, payload: Payload)

  enum Payload:
    case Literal(value: BigInt)
    case Operator(packets: Seq[Packet])

  case class Header(version: Int, tid: TypeId)

  enum TypeId:
    case sum, product, min, max, lit, gt, lt, eqq

  // Parsers

  val bit: Parser[Char] = char('0') | char('1')
  def intN(bits: Int): Parser[Int] = bit.listOfN(bits).map(_.mkString).map(Integer.parseInt(_, 2))

  val typeId: Parser[TypeId] = intN(3).map(TypeId.fromOrdinal)

  val literal: Parser[BigInt] =
    lazy val group: Parser[String] = (bit ** bit.listOfN(4).slice).flatMap {
      case ('1', four) => group.map(four + _)
      case ('0', four) => succeed(four)
    }
    group.map(BigInt(_, 2))

  val header: Parser[Header] = (intN(3) ** typeId).map(Header.apply(_, _))

  lazy val parsePacket: Parser[Packet] = header.flatMap { (header: Header) =>
    header.tid match
      case TypeId.lit => literal.map(l => Packet(header, Payload.Literal(l)))
      case other =>
        bit.flatMap {
          case '0' => intN(15).flatMap { packetSize => loc => parsePacket.many(Location(loc.slice(packetSize), 0)) }
          case '1' => intN(11).flatMap { numPackets => parsePacket.listOfN(numPackets) }
        }.map(p => Packet(header, Payload.Operator(p)))
  }

  // entrypoint for parsing the input
  val parsePacketRoot: Parser[Packet] = (parsePacket ** char('0').many).map(_._1)

  // Computations

  def versionSum(packet: Packet): Int = packet.header.version + packet.payload.match
    case Payload.Literal(_) => 0
    case Payload.Operator(ps) => ps.map(versionSum).sum

  def compute(packet: Packet): BigInt = packet.payload match
    case Payload.Literal(lit) => lit
    case Payload.Operator(ps) =>
      val sub = ps.map(compute)
      packet.header.tid match
      case TypeId.sum => sub.sum
      case TypeId.product => sub.product
      case TypeId.min => sub.min
      case TypeId.max => sub.max
      case TypeId.lit => ??? // can't happen
      case TypeId.gt => if sub.head > sub(1) then 1 else 0
      case TypeId.lt => if sub.head < sub(1) then 1 else 0
      case TypeId.eqq => if sub.head == sub(1) then 1 else 0

  // read lines, make binary, parse, run computations

  lines().map { line =>
    val binary: String = line.map { c =>
      Integer.parseInt(c.toString, 16).toBinaryString
    }.map(_.reverse.padTo(4, '0').reverse).mkString

    val packet: Either[String, Packet] = parsePacketRoot.run(binary)

    println(packet.map(versionSum)) // 940
    println(packet.map(compute)) // 13476220616073
  }
}

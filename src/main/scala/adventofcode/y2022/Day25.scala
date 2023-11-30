package adventofcode.y2022

@main
def Day25 = withResource("day25a.txt") {

  val subtractable = LazyList.from(1).map(size => java.lang.Long.parseUnsignedLong(Seq.fill(size)("2").mkString, 5))

  def snafu2dec(snafu: String): Long = {
    val raised = snafu.map {
      case c if c.isDigit => c.toString.toInt + 2
      case '-' => 1
      case '=' => 0
    }.mkString
    println(s"parsing: $snafu, $raised")
    val dec = java.lang.Long.parseUnsignedLong(raised, 5)
    dec - subtractable(snafu.size - 1)
  }
  
  def dec2snafu(dec: Long): String = {
    val base5Size = { // predict snafu size
      val b5 = java.lang.Long.toUnsignedString(dec, 5).toString
      b5.length + (b5.head match { case '3' | '4' => 1; case _ => 0 }) // 3 and 4 will overflow to next digit
    }
    val raised = dec + subtractable.apply(base5Size - 1)
    val base5 = java.lang.Long.toUnsignedString(raised, 5)
    base5.map {
      case '0' => '='
      case '1' => '-'
      case '2' => '0'
      case '3' => '1'
      case '4' => '2'
    }
  }

  val part1 = dec2snafu(lines().map(snafu2dec).sum)
  println(part1)

}

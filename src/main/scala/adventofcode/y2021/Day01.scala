package adventofcode.y2021

@main
def Day01 = {
  val input = Option(getClass.getResourceAsStream("/adventofcode/y2021/day01a.txt")).getOrElse(sys.error("Resource not found"))
  val lines = scala.io.Source.fromInputStream(input).getLines().toSeq

  val ints = lines.filterNot(_.isBlank).map(_.toInt)

  val resultA =
    ints
      .sliding(2)
      .count { case Seq(a, b) => a < b }    // 'safe' to match only 2-sized Seq here since we know the input is long enough

  println(resultA)

  val resultB = ints
    .sliding(3)
    .map(_.sum)
    .sliding(2)                             // same as A from here
    .count { case Seq(a, b) => a < b }

  println(resultB)

  /**
   *
   */
}

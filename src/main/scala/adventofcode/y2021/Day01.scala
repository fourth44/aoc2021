package adventofcode.y2021

@main
def Day01 = withResource("day01a.txt") {
  
  val resultA =
    intLines()
      .sliding(2)
      .count { case Seq(a, b) => a < b }    // 'safe' to match only 2-sized Seq here since we know the input is long enough

  println(resultA)

  val resultB = intLines()
    .sliding(3)
    .map(_.sum)
    .sliding(2)                             // same as A from here
    .count { case Seq(a, b) => a < b }

  println(resultB)

}

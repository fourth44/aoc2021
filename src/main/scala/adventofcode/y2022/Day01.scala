package adventofcode.y2022

@main
def Day01 = withResource("day01a.txt") {


  val grouped = List.unfold(lines())(lines => Option(lines.span(_.nonEmpty)).collect { case (a, b) if a.nonEmpty => (a, b.drop(1)) })
  val totals = grouped.map(_.map(_.toInt).sum)

  val solutionA = totals.max
  println(solutionA)

  val solutionB = totals.sorted.takeRight(3).sum
  println(solutionB)

}
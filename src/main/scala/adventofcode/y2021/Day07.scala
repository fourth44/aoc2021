package adventofcode.y2021

@main def Day07 = {
  val input = Option(getClass.getResourceAsStream("/adventofcode/y2021/day07a.txt")).getOrElse(sys.error("Resource not found"))
  val lines = scala.io.Source.fromInputStream(input).getLines().toSeq

  val crabs0Real = lines.head.split(',').toSeq.map(_.toInt)
  val crabs0Test = Seq(16,1,2,0,4,2,7,1,2,14)

  val crabs0 = crabs0Real // toggle

  // there is probably a smoarter way: median/average etc
  val resultA = (crabs0.min to crabs0.max).map { pos =>
    pos -> crabs0.map(c => math.abs(pos - c)).sum
  }.minBy(_._2)

  println(resultA)

  val resultB = (crabs0.min to crabs0.max).map { pos =>
    pos -> crabs0.map { c =>
      val d = math.abs(pos - c)
      (d * (d + 1)) / 2  // accumlative fuel consumption is distance^2/2, i.e. surface of triangle (integral of fuel consumption over time graph)
    }.sum
  }.minBy(_._2)

  println(resultB)


}

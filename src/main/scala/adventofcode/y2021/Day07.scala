package adventofcode.y2021

@main def Day07 = withResource("day07a.txt"){

  val crabs0Real = intsFirstLine()
  val crabs0Test = Seq(16,1,2,0,4,2,7,1,2,14)

  val crabs0 = crabs0Real // toggle

  // there is probably a smarter way: median/average etc
  val resultA = (crabs0.min to crabs0.max)
    .map { pos =>
      pos -> crabs0.map(c => math.abs(pos - c)).sum
    }
    .minBy(_._2)

  println(resultA)

  val resultB = (crabs0.min to crabs0.max).map { pos =>
    pos -> crabs0.map { c =>
      val d = math.abs(pos - c)
      (d * (d + 1)) / 2  // accumlative fuel consumption is distance^2/2, i.e. surface of triangle (integral of fuel consumption over time graph)
    }.sum
  }.minBy(_._2)

  println(resultB)


}

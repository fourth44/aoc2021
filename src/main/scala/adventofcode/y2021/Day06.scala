package adventofcode.y2021

@main def Day06 = {
  val input = Option(getClass.getResourceAsStream("/adventofcode/y2021/day06a.txt")).getOrElse(sys.error("Resource not found"))
  val lines = scala.io.Source.fromInputStream(input).getLines().toSeq

  val fish0 = lines.head.split(',').toSeq.map(_.toInt)
  val testFish = Seq(3,4,3,1,2)

  // 1st approach
  def step(fishStart: Seq[Int]): Seq[Int] = {
    fishStart.flatMap {
      case 0 => Seq(8, 6)
      case n => Seq(n - 1)
    }
  }

  val states = LazyList.iterate(fish0)(step)

  println(states.drop(80).head.size) // OK
  // println(states.drop(256).head.size) // to slow

  // 2nd approach

  val histogram0 = fish0.groupBy(identity).view.mapValues(_.size.toLong).toMap

  def step2(in: Map[Int, Long]): Map[Int, Long] = {
    (0 to 8).map {
      case 8 => 8 -> in.getOrElse(0, 0L)
      case 6 => 6 -> (in.getOrElse(0, 0L) + in.getOrElse(7, 0L))
      case c => c -> in.getOrElse(c + 1, 0L)
    }.toMap
  }

  val states2 = LazyList.iterate(histogram0)(step2)

  println(states2.drop(80).head.values.sum)
  println(states2.drop(256).head.values.sum)
}

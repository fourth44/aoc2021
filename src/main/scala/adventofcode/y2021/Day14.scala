package adventofcode.y2021

@main def Day14: Unit = withResource("day14a.txt") {
  val start = lines().head.toSeq
  val insertions = lines().drop(2).map { line => line.take(2) -> line.last }.toMap

  // initial, naïve, non-scalable approach:

  def step(template: Seq[Char]) =
   template.sliding(2).toSeq.flatMap {
     case pair@Seq(a, b) =>
      insertions.get(s"$a$b").fold(pair)(m => Seq(a, m))
   } :+ template.last

  def compute(steps: Int): Int =
    val histo = Iterator.iterate(start)(step).drop(steps).next().groupBy(identity).values.map(_.size).toSeq
    histo.max - histo.min

  val resultA = compute(10)
  require(resultA == 2587)

  // to slow!
  // val resultB = compute(40)

  // other approach:

  type Histo = Map[String, Long]

  val start2: Histo = start.sliding(2).map(_.toString).toSeq.groupBy(identity).view.mapValues(_.size.toLong).toMap

  val insertions2: Map[String, Seq[String]] = insertions.map { case (k, v) =>
    (k, Seq(s"${k(0)}$v", s"$v${k(1)}"))
  }.withDefaultValue(Seq())

  lazy val histoStream: LazyList[Histo] = start2 #:: histoStream.map { histo =>
    histo.toSeq.flatMap { case (key, amount) => insertions2(key).map(_ -> amount) }.groupMapReduce(_._1)(_._2)(_ + _)
  }

  def result(after: Int): Long =
    val histo = (histoStream(after).toSeq :+ (start.last.toString, 1L)).groupMapReduce(_._1.head)(_._2)(_ + _).values.toSeq
    histo.max - histo.min

  val resultA2 = result(10)
  val resultB = result(40)

  require(resultA2 == 2587)
  require(resultB == 3318837563123L)
}

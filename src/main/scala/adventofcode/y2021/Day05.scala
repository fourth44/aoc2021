package adventofcode.y2021

@main def Day05(): Unit = {
  val input = Option(getClass.getResourceAsStream("/adventofcode/y2021/day05a.txt")).getOrElse(sys.error("Resource not found"))
  val lines = scala.io.Source.fromInputStream(input).getLines().toSeq

  val ventLines: Seq[Segment[Int]] =
    lines.map { case r"(\d+)${AsInt(x1)},(\d+)${AsInt(y1)} \-\> (\d+)${AsInt(x2)},(\d+)${AsInt(y2)}" => Segment(Vect2D(x1, y1), Vect2D(x2, y2))}

  val horzVert: Seq[Segment[Int]] = ventLines.filter(_.isHorzVert)

  def intersections(ventLines: Seq[Segment[Int]]) = {
    val intersectionListWithLines = for {
      l1 <- ventLines
      l2 <- ventLines // cartesian product -- we test to many here
      if l1 != l2
      i <- l1.intersectSegment(l2).toSeq ++ l1.overlap(l2).toSeq.flatMap(_.integralPoints)
    } yield {
      i -> Seq(l1, l2)
    }
    intersectionListWithLines.groupMapReduce(_._1)(_._2)((a, b) => (a ++ b).distinct)
  }

  val resultA = intersections(horzVert).filter(_._2.sizeIs >= 2)
  println(resultA.size)

  val resultB = intersections(ventLines).filter(_._2.sizeIs >= 2) // same computation, other input
  println(resultB.size)

}

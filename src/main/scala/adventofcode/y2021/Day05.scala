package adventofcode.y2021

@main def Day05(): Unit = {
  val input = Option(getClass.getResourceAsStream("/adventofcode/y2021/day05a.txt")).getOrElse(sys.error("Resource not found"))
  val lines = scala.io.Source.fromInputStream(input).getLines().toSeq

  val ventLines: Seq[Segment[Int]] =
    lines.map { case r"(\d+)${AsInt(x1)},(\d+)${AsInt(y1)} \-\> (\d+)${AsInt(x2)},(\d+)${AsInt(y2)}" => Segment(Vect2D(x1, y1), Vect2D(x2, y2))}

  val horzVert: Seq[Segment[Int]] = ventLines.filter(_.isHorzVert)

  def intersections(ventLines: Seq[Segment[Int]]): Map[Vect2D[Int], Seq[Segment[Int]]] = {
    val intersectionListWithLines: Seq[(Vect2D[Int], Seq[Segment[Int]])] =
      for
        i <- ventLines.indices
        j <- i + 1 until ventLines.size // 'triangle' of all unique combinations
        (l1, l2) = (ventLines(i), ventLines(j))
        intersections = l1.intersectSegment(l2).toSeq
        overlap = l1.overlap(l2).toSeq.flatMap(_.integralPoints)
        i <- intersections ++ overlap
      yield
        i -> Seq(l1, l2)
    intersectionListWithLines.groupMapReduce(_._1)(_._2)((a, b) => (a ++ b).distinct) // combine entries on same points
  }

  val resultA = intersections(horzVert).filter(_._2.sizeIs >= 2)
  println(resultA.size)

  val resultB = intersections(ventLines).filter(_._2.sizeIs >= 2) // same computation, other input
  println(resultB.size)

}

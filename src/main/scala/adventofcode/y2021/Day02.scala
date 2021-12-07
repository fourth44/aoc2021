package adventofcode.y2021


@main
def Day02 = {
  val input = Option(getClass.getResourceAsStream("/adventofcode/y2021/day02a.txt")).getOrElse(sys.error("Resource not found"))
  val lines = scala.io.Source.fromInputStream(input).getLines().toSeq

  enum Dir(val vect: Vect2D[Int]):
    case forward extends Dir(Vect2D(1, 0))
    case down extends Dir(Vect2D(0, 1))
    case up extends Dir(Vect2D(0, -1))
  
  val dirsAmounts: Seq[(Dir, Int)] = lines.map { case r"(\w+)${dir} (\d+)${amount}" => Dir.valueOf(dir) -> amount.toInt }

  val Vect2D(endX, endY) = dirsAmounts.foldLeft(Vect2D(0, 0)) { case (pos, (dir, amnt)) => pos + dir.vect * amnt }

  // result A
  println(endX * endY)

  val (Vect2D(endXb, endYb), _) = dirsAmounts.foldLeft((Vect2D(0, 0), 0)) { case ((pos, angle), (dir, amnt)) =>
    dir match
      case Dir.up => (pos, angle - amnt)
      case Dir.forward => (pos + dir.vect * amnt + Dir.down.vect * angle * amnt, angle)
      case Dir.down => (pos, angle + amnt)
  }

  // result B
  println(endXb * endYb)

}

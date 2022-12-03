package adventofcode.y2022

@main
def Day03 = withResource("day03a.txt") {
  val overlaps = lines()
    .map(line => line.splitAt(line.length/2))
    .map((a, b) => a.toSet.intersect(b.toSet))

  def priority(c: Char): Int = c match
    case lc if lc.isLower => c - 'a' + 1
    case uc => uc - 'A' + 27

  // part 1
  println(overlaps.map(_.head).map(priority).sum)

  val badges = lines()
    .grouped(3).toSeq
    .map(_.map(_.toSet).reduce(_ intersect _).head)

  // part 2
  println(badges.map(priority).sum)

}
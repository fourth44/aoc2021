package adventofcode.y2022

@main
def Day04 = withResource("day04a.txt") {

  val segmentPairs = lines()
    .map { case r"(\d+)${AsInt(a)}-(\d+)${AsInt(b)},(\d+)${AsInt(c)}-(\d+)${AsInt(d)}" => (a, b) -> (c, d) }

  def contain(one: (Int, Int), two: (Int, Int)) = (one, two) match
    case ((a, b), (c, d)) if a <= c && b >= d || a >= c && b <= d => true
    case _ => false

  println(segmentPairs.count(contain))

  def overlap(one: (Int, Int), two: (Int, Int)) = (one, two) match
    case ((a, b), (c, d)) if b >= c && b <= d || a >= c && a <= d => true;
    case _ => contain(one, two)

  println(segmentPairs.count(overlap))

}
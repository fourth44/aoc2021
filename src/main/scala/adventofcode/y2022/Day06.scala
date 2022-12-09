package adventofcode.y2022

@main
def Day06 = withResource("day06a.txt") {

  def markerIndex(str: String, size: Int) = str.sliding(size).indexWhere(_.distinct.sizeIs == size) + size
  println(markerIndex(lines().head, 4))
  println(markerIndex(lines().head, 14))

}
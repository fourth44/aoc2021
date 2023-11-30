package adventofcode.y2022

def rotate[T](seq: Seq[T], n: Int) = // 'shiftLeft', a bit overgeneric for this use case
  val shift = n % seq.size
  val count = if (shift > 0) shift else seq.size + shift
  seq.drop(count) ++ seq.take(count)

@main
def Day20 = withResource("day20a.txt") {

  val key = 811589153L
  val start = intLines().map(_.toLong)


  def mix(in: Seq[Long], mult: Long = 1L, times: Int = 1): Seq[Long] =
    val indexed = in.map(_ * mult).zipWithIndex
    (1 to times).foldLeft(indexed)((acc, _) => in.indices.foldLeft(acc)(mixStep)).map(_._1)

  def mixStep(in: Seq[(Long, Int)], whichOrigIndex: Int): Seq[(Long, Int)] =
    val from = in.indexWhere(_._2 == whichOrigIndex)
    val which = in(from)._1
    if (which % (in.size - 1) == 0) in else {
      val to = scala.math.floorMod(from + which + in.size - 2, in.size - 1) + 1 // if to is 0, wrap to end!
      val Seq(first, last) = Seq(from, to.toInt).sorted
      val rot = if (from == first) 1 else -1
      in.patch(first, rotate(in.slice(first, last + 1), rot), last + 1 - first)
    }

  def result(seq: Seq[Long]) =
    val i0 = seq.indexOf(0)
    val indices = Seq(1000, 2000, 3000).map(_ + i0).map(_ % seq.size)
    indices.map(seq.apply).sum

  println(result(mix(start))) // part 1
  println(result(mix(start, key, 10))) // part 2
}

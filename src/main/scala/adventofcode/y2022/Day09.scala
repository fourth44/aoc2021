package adventofcode.y2022

import adventofcode.y2021.Vect2D

type Vect = Vect2D[Int]
val Vect = Vect2D

enum Dir(val vec: Vect):
  case R extends Dir(Vect(1, 0))
  case D extends Dir(Vect(0, 1))
  case L extends Dir(Vect(-1, 0))
  case U extends Dir(Vect(0, -1))

@main
def Day09 = withResource("day09a.txt") {


  val parsed: Seq[(Dir, Int)] =
    lines().map { case s"${dir} ${AsInt(dist)}" => Dir.valueOf(dir) -> dist }

  val headPositions: Seq[Vect] =
    parsed
      .flatMap((dir, amount) => Seq.fill(amount)(dir.vec)) // now a list of single step movements, each as a Vect
      .scanLeft(Vect(0, 0))(_ + _)                         // now applied sequentially from the start position

  def follow(head: Vect, tail: Vect): Vect = // given an already moved rope piece and it's yet unmoved next rope piece, where will the next rope piece move?
    val diff = head + tail * -1
    val move = if (diff.seq.exists(_.abs >= 2)) diff.unit else Vect(0, 0)
    tail + move

  def followPath(head: Seq[Vect]): Seq[Vect] = // given the movement path of one piece of rope, how will the next rope piece move along?
    head.scanLeft(Vect(0, 0))((t, h) => follow(h, t))

  val tailPositions: Seq[Vect] =
    followPath(headPositions)

  println(tailPositions.distinct.size)

  val ropeMovements: LazyList[Seq[Vect]] = // an infinite rope, for each segment you get the path it follows
    LazyList.iterate(headPositions)(followPath)

  println(ropeMovements(9).distinct.size) // beyond 9 nothing will be computed
}

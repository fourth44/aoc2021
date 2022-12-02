package adventofcode.y2021
import scala.compiletime.ops.int.*

@main def Day20: Unit = withResource("day20a.txt") {

  type Bool = Int // 0 | 1, doesn't work well
  inline def bool(c: Char): Bool = if c == '#' then 1 else 0

  val algo = lines().head.map(bool(_))
  val img0: IndexedSeq[IndexedSeq[Bool]] = lines().drop(2).toIndexedSeq.map(_.map(c => bool(c)))
  def allPoints(steps: Int): Seq[Vect2D[Bool]] = (-steps until img0.size + steps).flatMap(y => (-steps until img0.head.size + steps).map(x => Vect2D(x, y)))
  val img0Map = allPoints(0).map(v => v -> img0(v.y)(v.x)).toMap.withDefaultValue(0)

  val region: Seq[Vect2D[Int]] = // all 8 neighbours
    (-1 to 1).flatMap(y => (-1 to 1).map(x => Vect2D(x, y))) // .filterNot(v => v.x == 0 && v.y == 0)

  val store0 = Store[Vect2D[Int], Bool](img0Map.apply, Vect2D(0, 0))


  def bools2int(nine: Seq[Bool]): Int = nine.reverse.zipWithIndex.map { case (bool, i) => bool * 1 << i }.sum

  def step(store: Grid[Bool]): Grid[Bool] = store.extend(_.experiment(p => region.map(_ + p))).map(bools2int).map(algo.apply)

  val resultImgA = step(step(store0))
  val resultA = resultImgA.experiment(_ => allPoints(2)).count(_ == 1)
  println(resultA)


  val resultImgB = Iterator.iterate(store0)(s => step(s)).drop(50).next()
  val resultB = resultImgB.experiment(_ => allPoints(50)).count(_ == 1)
  println(resultB)


}

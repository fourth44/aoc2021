package adventofcode.y2022

@main
def Day23 = withResource("day23a.txt") {

  val elves0: Set[Vect] = lines().zipWithIndex.flatMap((line, y) => line.zipWithIndex.collect { case ('#', x) => Vect(x, y) }).toSet

  val directions: Seq[Vect] = Seq(Vect(-1, -1), Vect(0, -1), Vect(1, -1), Vect(1, 0), Vect(1, 1), Vect(0, 1), Vect(-1, 1), Vect(-1, 0), Vect(-1, -1))
  val nswe0 = Seq(0, 4, 6, 2).map(n => directions.slice(n, n + 3))

  // alles leeg: niet bewegen
  // anders in de eerste richting waarin het leeg is (ook diagonaal)

  def rounds = Iterator.iterate((elves0, nswe0)) { case (elves, nswe) =>
    val (stationary1, trymoving) = elves.groupBy { elf =>
      val dirsNonEmpty = nswe.map(dirs => dirs.map(dir => elf + dir)).map(d => d(1) -> d.exists(elves))
      Some(dirsNonEmpty).filter(_.exists(_._2)).flatMap(_.collectFirst { case (d, false) => d })
    }.partitionMap((whereto, elf) => whereto.map(_ -> elf).toRight(elf))
    val (stationary2, moving) = trymoving.partitionMap((where, who) => Either.cond(who.size == 1, where, who))
    (stationary1.toSet.flatten ++ stationary2.toSet.flatten ++ moving.toSet) -> rotate(nswe, 1)
  }

  def box(elves: Set[Vect]): (Vect, Vect) = {
    val xs = elves.map(_.x)
    val ys = elves.map(_.y)
    Vect(xs.min, ys.min) -> Vect(xs.max, ys.max)
  }

  def emptyInBox(elves: Set[Vect]): Int = {
    val (Vect(xmin, ymin), Vect(xmax, ymax)) = box(elves)
    (xmax - xmin + 1) * (ymax - ymin + 1) - elves.size
  }

  def show(elves: Set[Vect]): String = {
    val (Vect(xmin, ymin), Vect(xmax, ymax)) = box(elves)
    (ymin to ymax).map { y =>
      (xmin to xmax).map { x =>
        if (elves(Vect(x, y))) "#" else "."
      }.mkString
    }.mkString("\n")
  }

  // rounds.map(r => show(r._1)).take(11).map(_ + "\n").foreach(println)

  val r10 = rounds.drop(10).next()._1
  println(emptyInBox(r10)) // 2731 te laag

  val firstNoMoveIndex = rounds.zipWithIndex.sliding(2).collectFirst { case Seq(((a, _), i), ((b, _), _)) if a == b => i + 1 }
  println(firstNoMoveIndex)
}

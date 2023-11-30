package adventofcode.y2022

@main
def Day10 = withResource("day10a.txt") {

  enum Op:
    case Noop
    case Add(amount: Int)

  val instr: Seq[Op] = lines().flatMap {
    case s"addx ${AsInt(add)}" => Seq(Op.Noop, Op.Add(add))
    case "noop" => Seq(Op.Noop)
  }

  val history: Seq[Int] = instr.scanLeft(1) { (x, insr) => insr match
    case Op.Noop => x
    case Op.Add(n) => x + n
   }
  val part1 = Seq(20, 60, 100, 140, 180, 220).map(c => history(c-1) * c)
  println(part1.sum)

  history
    .zipWithIndex
    .map { case (x, i) => if ((x - (i % 40)).abs <= 1) "##" else "  " } // double-width pixels are easier to read
    .grouped(40)
    .map(_.mkString)
    .foreach(println) // part 2
}
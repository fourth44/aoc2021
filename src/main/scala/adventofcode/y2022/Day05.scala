package adventofcode.y2022

@main
def Day05 = withResource("day05a.txt") {

  type Stacks = Seq[Seq[Char]]

  val stackLines = lines().takeWhile(_.trim.startsWith("["))
    .map { _.grouped(4).map(_(1)).toSeq }

  val moves = lines().drop(stackLines.size + 2)
    .map { case r"move (\d+)${AsInt(amount)} from (\d+)${AsInt(from)} to (\d+)${AsInt(to)}" => (amount, from, to)}

  val width = stackLines.map(_.size).max
  val stacks: Stacks = stackLines.map(_.padTo(width, ' ')).transpose.map(_.filterNot(_.isWhitespace))

  def step(begin: Stacks, multi: Boolean)(amount: Int, from: Int, to: Int): Stacks = {
    val (take, rem) = begin(from - 1).splitAt(amount)
    begin.updated(from - 1, rem).updated(to - 1, (if (multi) take else take.reverse) ++ begin(to - 1))
  }

  def topAfter(multi: Boolean) = moves.foldLeft(stacks)(step(_, multi).tupled(_)).map(_.head).mkString

  println(topAfter(multi = false)) // part 1
  println(topAfter(multi = true))  // part 2

}

package adventofcode.y2022

@main
def Day02 = withResource("day02a.txt") {

  enum RPS(val score: Int):
    case Rock extends RPS(1)
    case Paper extends RPS(2)
    case Scissors extends RPS(3)
    case Spock extends RPS(4)
    case Lizard extends RPS(5)

  // TODO should split up in loose/dwaw/win and compute score seperately
  def outcome(other: RPS, you: RPS): Int = math.floorMod(you.score - other.score, 5) match
    case 0 => // zero distance: draw
      3 + you.score
    case i if i % 2 == 1 => // uneven distance: win
      6 + you.score
    case _ => // even distance: loose
      0 + you.score

  def parse1(s: String): RPS = s match
    case "A" | "X" => RPS.Rock
    case "B" | "Y" => RPS.Paper
    case "C" | "Z" => RPS.Scissors
    case "D" => RPS.Spock
    case "E" => RPS.Lizard

  val strategies1 = lines()
    .map { case r"(\w)${one} (\w)${two}" => parse1(one) -> parse1(two) }

  // Part 1:
  println(strategies1.map(outcome.tupled).sum) // 13052

  enum Result:
    case Loose, Draw, Win

  def parse2(s: String): Result = s match
    case "X" => Result.Loose; case "Y" => Result.Draw; case "Z" => Result.Win

  def pick(other: RPS, result: Result): RPS = RPS.fromOrdinal(math.floorMod(other.score + result.ordinal + 1, 3))

  val strategies2 = lines()
    .map { case r"(\w)${one} (\w)${two}" => parse1(one) -> parse2(two) }

  // Part 2:
  println(strategies2.map((rps, result) => outcome(rps, pick(rps, result))).sum) // 13693

  // results for part 1 and 2 still work after adding lizard and spock

  println(outcome(other = RPS.Rock, you = RPS.Spock)) // spock vaporizes rock, you win: 6+4=10

}
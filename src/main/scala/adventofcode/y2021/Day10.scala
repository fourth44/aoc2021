package adventofcode.y2021

@main def Day10 = withResource("day10a.txt") {

  enum ParseResult:
    case Incomplete(expected: Seq[Char])
    case Wrong(offender: Char)
    case Ok

  val pairs = Seq('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  def isOpener(c: Char) = pairs.map(_._1).contains(c)
  object Closer:
    def unapply(c: Char): Option[Char] = pairs.collectFirst { (opener, `c`) => opener }

  def doLine(line: String): ParseResult =
    // starting from the input string and and empty stack, repeatedly:
    (line.toSeq -> List[Char]()).unfold { // (Left = again, Right = stop)
      case (c +: t, stack) if isOpener(c) => // push new opening char to stack
        Left((t, c :: stack))
      case ((c@Closer(opener)) +: t, top :: stack) if top == opener => // closing char matching top of stack: pop
        Left((t, stack))
      case foo@((c@Closer(_)) +: _, _) => // otherwise closing char is Wrong
        Right(ParseResult.Wrong(c))
      case (Seq(), List()) => // input and stack empty: done!
        Right(ParseResult.Ok)
      case (Seq(), incomplete) => // imput empty, but stack not: incomplete!
        Right(ParseResult.Incomplete(incomplete.map(o => pairs.collectFirst { case (`o`, c) => c }.get)))
    }

  val parsedLines = lines().map(doLine)

  val score = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  val resultA = parsedLines.collect { case ParseResult.Wrong(c) => score(c) }.sum

  println(resultA)

  val score2 = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  val scoresB = parsedLines
    .collect { case ParseResult.Incomplete(missing) => missing.foldLeft(0L) { case (acc, c) => acc * 5 + score2(c) } }
    .sorted

  val resultB = scoresB(scoresB.size / 2)


  println(resultB)

}

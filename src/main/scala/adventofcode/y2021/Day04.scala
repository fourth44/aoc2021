package adventofcode.y2021

@main def Day04(): Unit =  {

  type Board = Seq[Seq[(Int, Boolean)]]

  val input = Option(getClass.getResourceAsStream("/adventofcode/y2021/day04a.txt")).getOrElse(sys.error("Resource not found"))
  val lines = scala.io.Source.fromInputStream(input).getLines().toSeq

  val numbers = lines(0).split(',').toSeq.map(_.toInt)
  val boards: Seq[Board] = lines.drop(2).grouped(6).map(_.take(5).map(_.grouped(3).take(5).toSeq.map(_.trim.toInt -> false))).toSeq

  def boardDone(board: Board): Boolean = Seq(board, board.transpose).exists(_.exists(_.forall(_._2)))

  def mark(board: Board, num: Int): Board =
    board.map(_.map {
      case (`num`, _) => (num, true)
      case other => other
    })

  def score(board: Board, num: Int): Int = board.flatten.collect { case (i, false) => i }.sum * num
  
  val timeline: Seq[(Int, Seq[Board])] =
    numbers zip numbers.scanLeft(boards) { (brds, num) =>  brds.map(mark(_, num)) }.drop(1) // drop initial state

  val resultA = timeline.flatMap { case (num, boards) =>
    boards.find(boardDone).map(score(_, num))
  }.head

  println(resultA)

  // go backward, find the index at which the firt board doesn't win anymore
  val (timeBeforeLastBoardWinning, lastBoardNumber) = timeline.zipWithIndex.reverse.flatMap { case ((num, brds), timeIndex) =>
    brds.zipWithIndex.collectFirst { case (b, boardIndex) if !boardDone(b) => (timeIndex, boardIndex) }
  }.head

  val resultB = {
    val (num, boards) = timeline(timeBeforeLastBoardWinning + 1)
    score(boards(lastBoardNumber), num)
  }

  println(resultB)



}

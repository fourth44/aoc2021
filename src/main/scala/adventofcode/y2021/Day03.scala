package adventofcode.y2021

import scala.util.chaining.*

def bin2dec(binary: String): Int = Integer.parseInt(binary, 2)

@main
def Day03 = {
  val input = Option(getClass.getResourceAsStream("/adventofcode/y2021/day03a.txt")).getOrElse(sys.error("Resource not found"))
  val lines = scala.io.Source.fromInputStream(input).getLines().toSeq
  val testLines = """00100
                |11110
                |10110
                |10111
                |10101
                |01111
                |00111
                |11100
                |10000
                |11001
                |00010
                |01010""".stripMargin.split("\n").toSeq // to try examples from assignment

  val transposed = lines.transpose

  val histogramByCol: Seq[Map[Int, Char]] = transposed.map(_.groupBy(identity).map { case (k, v) => (v.size, k) })

  val gamma = histogramByCol.map(_.maxBy(_._1)._2).mkString
  val epsilon = histogramByCol.map(_.minBy(_._1)._2).mkString

  // result A
  println(bin2dec(gamma) * bin2dec(epsilon))

  // 2nd part

  def compute(f: Seq[Seq[String]] => Seq[String]) = (0 until lines.head.length).foldLeft(lines) { case (candidates, column) =>
    candidates.groupBy(_(column)).toSeq.sortBy { case (c, l) => (l.length, c) }.map(_._2).pipe(f)
  }

  val Seq(oxygen) = compute(_.last)
  val Seq(co2) = compute(_.head)

  // result B
  println(bin2dec(oxygen) * bin2dec(co2))

}

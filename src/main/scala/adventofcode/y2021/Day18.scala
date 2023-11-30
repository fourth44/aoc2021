package adventofcode.y2021

import Parser.*
import scala.annotation.tailrec



@main def Day18: Unit = withResource("day18a.txt"){

  enum SnailFishNumber {
    case Pair(one: SnailFishNumber, two: SnailFishNumber)
    case Num(num: Int)

    infix def add(snf2: SnailFishNumber) = SnailFishNumber.Pair(this, snf2)

    def fold[B](num: Int => B)(combine: (B, B) => B): B = this match
      case SnailFishNumber.Pair(a, b) => combine(a.fold(num)(combine), b.fold(num)(combine))
      case SnailFishNumber.Num(n) => num(n)

    def magnitude: Int = fold(identity)((a, b) => a * 3 + b * 2)

    override def toString: String = fold(_.toString)((a, b) => s"[$a,$b]")
  }

  // alternative representation

  enum Dir:
    case L, R

  type Path = (Seq[Dir], Int)
  type Paths = Seq[Path]

  // parser
  lazy val snailFishNumber: Parser[SnailFishNumber] =
    regex("[0-9]".r).map(n => SnailFishNumber.Num(n.toInt)) | bracketed((snailFishNumber ** string(",") ** snailFishNumber).map {
      case ((one, _), two) => SnailFishNumber.Pair(one, two)
    })

  // converters
  def num2paths(snf: SnailFishNumber): Paths = snf.fold[Paths](n => Seq((Seq(), n)))(
    (one, two) => one.map { (p, i) => (Dir.L +: p, i) } ++ two.map { case (p, i) => (Dir.R +: p, i) }
  )

  def paths2num(paths: Paths): SnailFishNumber =
    val map = paths.groupMap(_._1.headOption) { case (p, i) => (p.drop(1), i) }
    map.toSeq.sortBy(_._1.map(_.ordinal)) match {
      case Seq((Some(Dir.L), p1), (Some(Dir.R), p2)) =>
        SnailFishNumber.Pair(paths2num(p1), paths2num(p2))
      case Seq((None, Seq((Seq(), i)))) =>
        SnailFishNumber.Num(i)
    }

  // explode/split/reduce

  type Step = Paths => (Paths, Boolean) // transformer that informs whether anything has changed
  extension (step1: Step)
    def or(step2: Step): Step = paths => step1(paths) match
      case (_, false) => step2(paths);  // first did not fire, try second
      case other => other               // first did fire, use result

  def step(splitAfter: Path => Boolean)(f: ((Paths, Paths)) => Paths): Step =
    paths => paths.indexWhere(splitAfter) match
      case -1 => paths -> false
      case n => f(paths.splitAt(n + 1)) -> true

  def carry(paths: Paths, n: Int): Paths = paths match // explode helper
    case (p, i) +: t => (p, i + n) +: t
    case Seq() => Seq()

  def explode: Step = step(_._1.size >= 5) { case (before :+ ((p, l)), (_, r) +: after) => carry(before.reverse, l).reverse ++ ((p.init, 0) +: carry(after, r)) }

  def split: Step = step(_._2 >= 10) { case (before :+ ((p, i)), after) =>  before ++ ((p :+ Dir.L, (i / 2d).floor.toInt) +: (p :+ Dir.R, (i / 2d).ceil.toInt) +: after) }

  @tailrec def reducePaths(paths: Paths): Paths =
    (explode or split)(paths) match
      case (p2, true) => reducePaths(p2)
      case (p2, false) => p2

  def reduce(snf: SnailFishNumber): SnailFishNumber = paths2num(reducePaths(num2paths(snf))) // convert back and forth

  def sum(snfs: Seq[SnailFishNumber]): SnailFishNumber = snfs.reduce((a, b) => { reduce(a add b) }) // assume snfs non-empty! there is no 'zero'

  val parsed = lines().map(snailFishNumber.run(_).getOrElse(???))

  val resultA = sum(parsed)
  println(resultA.magnitude)

  val resulB = (parsed.combinations(2).map(sum) ++ parsed.reverse.combinations(2).map(sum)).toSeq.map(_.magnitude).max
  println(resulB)

}

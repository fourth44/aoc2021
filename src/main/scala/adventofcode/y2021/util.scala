package adventofcode.y2021

import scala.annotation.tailrec
import scala.util.matching.Regex

// helper for pattern matching on regexes without predefining the regex first
implicit class RegexContext(sc: StringContext):
  def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)

object AsInt:
  def unapply(s: String): Option[Int] = s.toIntOption

extension [S](s: S)
  // repeatedly iterate over an initial state until it emits a desired value
  @tailrec
  def unfold[A](f: S => Either[S, A]): A =
    f(s) match
      case Left(s2) => s2.unfold(f)
      case Right(a) => a

// helpers to reduce file reading boilerplate (which already hardly existed anyway...)

opaque type Source = () => scala.io.Source

def withResource[A](fileName: String)(thunk: Source ?=> A): A = {
  val mkSrc = () => Option(getClass.getResourceAsStream(s"/adventofcode/y2021/${fileName}")).map { is =>
    scala.io.Source.fromInputStream(is)
  }.getOrElse(sys.error("Resource not found"))
  thunk(using mkSrc)
}

def lines()(using src: Source): Seq[String] = src().getLines().toSeq

def intsFirstLine(): Source ?=> Seq[Int] = lines().head.split(',').toSeq.map(_.toInt)
def intLines(): Source ?=> Seq[Int] = lines().filterNot(_.isBlank).map(_.toInt)


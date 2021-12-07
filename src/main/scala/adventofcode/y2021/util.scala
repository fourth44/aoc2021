package adventofcode.y2021

import scala.util.matching.Regex

// helper for pattern matching on regexes without predefining the regex first
implicit class RegexContext(sc: StringContext):
  def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)

object AsInt:
  def unapply(s: String): Option[Int] = s.toIntOption

// typeclass for division which Numeric doesn't handle.
trait CanDiv[A]:
  def div(a: A, B: A): A
  extension (a: A) def / (b: A) = div(a, b)

given CanDiv[Int] with
  def div(a: Int, b: Int) = a / b

given CanDiv[Double] with
  def div(a: Double, b: Double) = a / b

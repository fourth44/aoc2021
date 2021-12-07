package adventofcode.y2021

import scala.util.matching.Regex

// helper for pattern matching on regexes without predefining the regex first
implicit class RegexContext(sc: StringContext):
  def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)

object AsInt:
  def unapply(s: String): Option[Int] = s.toIntOption

package adventofcode.y2022

import scala.annotation.tailrec
import scala.util.matching.Regex

// helper for pattern matching on regexes without predefining the regex first
extension (sc: StringContext)
  def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)

object AsInt:
  def unapply(s: String): Option[Int] = s.toIntOption

extension [S](s: S)
  // repeatedly iterate over an initial state until it emits a desired value
  @tailrec
  def iterate[A](f: S => Either[S, A]): A =
    f(s) match
      case Left(s2) => s2.iterate(f)
      case Right(a) => a

extension [K,V1](map: Map[K, V1]) def intersect[V2](map2: Map[K, V2]): Map[K, (V1, V2)] =
  map.keySet.intersect(map2.keySet).map(k => k -> (map(k), map2(k))).toMap


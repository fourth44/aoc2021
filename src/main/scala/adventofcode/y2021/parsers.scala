package adventofcode.y2021

import scala.util.matching.Regex

type Parser[+A] = Location => Result[A]

case class Location(input: String, offset: Int = 0):
  def slice(n: Int) = input.substring(offset, offset + n)

enum Result[+A]:
  case Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case Failure(get: String) extends Result[Nothing]
  def advanceSuccess(n: Int): Result[A] = this match
    case Success(a, m) => Success(a, n + m)
    case _ => this

def succeed[A](a: A): Parser[A] = _ => Result.Success(a, 0)

extension[A](p: Parser[A]) {
  def run(input: String): Either[String, A] =
    val loc = Location(input, 0)
    p(loc) match
      case Result.Success(a, consumed@_) => Right(a)
      case Result.Failure(m) => Left(m)

  def map[B](f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

  def flatMap[B](f: A => Parser[B]): Parser[B] = loc => {
    p(loc) match
      case Result.Success(a, charsConsumed) =>
        //We have to advance the success by the initial characters consumed, otherwise the characters consumed by p are forgotten.
        f(a)(Location(loc.input, loc.offset + charsConsumed)).advanceSuccess(charsConsumed)
      case f@Result.Failure(_) => f

  }

  def **[B](p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def |(p2: => Parser[A]): Parser[A] = loc => p(loc) match
    case Result.Failure(_) => p2(loc)
    case s => s

  def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = (p ** p2).map(f.tupled)

  def listOfN(n: Int): Parser[List[A]] =
    if n <= 0 then succeed(Nil)
    else p.map2(p.listOfN(n - 1))(_ :: _)

  def many: Parser[List[A]] = p.map2(p.many)(_ :: _) | succeed(Nil)

  def slice: Parser[String] =
    l => p(l) match
      case Result.Success(_, n) => Result.Success(l.slice(n), n)
      case f @ Result.Failure(_) => f

  def opt: Parser[Option[A]] =
    p.map(Some(_)) | succeed(None)
}


def string(s: String): Parser[String] = loc => {
  val toProcess = loc.input.substring(loc.offset)
  if (toProcess.startsWith(s)) Result.Success(s, s.length)
  else Result.Failure(s"Input at ${loc.offset} does not start with $s")
}

def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

// above the subset used by Day16

def regex(r: Regex): Parser[String] = loc => {
  val toProcess = loc.input.substring(loc.offset)
  r.findPrefixOf(toProcess) match 
    case None =>
      def toProcess = loc.input.substring(loc.offset)
      Result.Failure(s"$toProcess does not match regex $r")
    case Some(m) => Result.Success(m, m.length)
}

def whitespace: Parser[String] = regex("\\s*".r)

def eof: Parser[String] = loc => regex("\\z".r)(loc) match 
  case Result.Failure(_) =>
    val leftOver = loc.input.substring(loc.offset)
    Result.Failure(s"Expected E.O.F. but found $leftOver")
  case s => s



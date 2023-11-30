package adventofcode.y2022

import adventofcode.y2021.Parser
import adventofcode.y2021.Parser.*
import adventofcode.y2021.Parser
import adventofcode.y2021.Parser.*
import adventofcode.y2021.Segment

@main
def Day14 = withResource("day14a.txt") {

  val parseVect: Parser[Vect] = (num ** (string(",") *> num)).map((x, y) => Vect(x, y))
  val parseLine: Parser[Seq[Vect]] = repsep(string(" -> "), parseVect)

  val vects: Seq[Seq[Vect]] = lines().map(parseLine.run(_).toOption.get)
  val segments = vects.map(line => line.sliding(2).map { case Seq(a, b) => Segment(a, b) }.toSeq)
  val startPoints = segments.flatten.flatMap(_.integralPoints).toSet

  val source = Vect(500,0)
  val lowest = startPoints.map(_.y).max

  case class State(stationary: Set[Vect], falling: Vect)

  val fallOrder = Seq(Vect(0, 1), Vect(-1, 1), Vect(1, 1))

  def endState(stop: State => Boolean, candidateFilter: Vect => Boolean): Set[Vect] =
    State(startPoints, source).iterate { case state@State(stationary, falling) =>
      if (stop(state)) Right(stationary) else Left {
        fallOrder
          .map(_ + falling)
          .filter(candidateFilter)
          .find(!stationary(_))
          .map(moveInto => State(stationary, moveInto))
          .getOrElse(State(stationary + falling, source))
      }
    }

  // stop when any sand falls into the abyss
  println(endState(_.falling.y > lowest, _ => true).size - startPoints.size)

  // stop when when the source is blocked
  println(endState(_.stationary(source), _.y < lowest + 2).size - startPoints.size)
}

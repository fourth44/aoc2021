package adventofcode.y2022
import scala.util.chaining.*

@main
def Day21 = withResource("day21a.txt") {

  type Monkey = String // not extra type safe, but a bit more readable
  type Op = String

  enum Shout:
    case Number(i: Long)
    case Operation(lhs: Monkey, rhs: Monkey, op: Op) // lhs, rhs: left/right hand side

  val input: Map[Monkey, Shout] = lines().map {
    case s"$who: ${AsInt(n)}"   => who -> Shout.Number(n)
    case s"$who: $lhs $op $rhs" => who -> Shout.Operation(lhs, rhs, op)
  }.toMap

  // a OP b = ?, i.e. regular math
  def executeOp(s: Op): (Long, Long) => Long = s match
    case "-" => _ - _
    case "+" => _ + _
    case "*" => _ * _
    case "/" => _ / _

  def predictMonkeyResult(who: Monkey, shoutings: Map[Monkey, Shout] = input): Long =
    shoutings(who) match
      case Shout.Number(n) => n
      case Shout.Operation(lhs, rhs, op) => executeOp(op)(predictMonkeyResult(lhs, shoutings), predictMonkeyResult(rhs, shoutings))

  println(predictMonkeyResult("root")) // part 1

  // ? OP b = c, i.e. what should 'a' be, given 'b' and 'c'?
  def inverseExecuteOpLhs(s: Op): (Long, Long) => Long = s match
    case "-" => _ + _
    case "+" => (b, c) => c - b
    case "*" => (b, c) => c / b
    case "/" => _ * _

  // a OP ? = c, i.e. what should 'b' be, given 'a' and 'c'?
  def inverseExecuteOpRhs(s: Op): (Long, Long) => Long = s match
    case "-" => _ - _
    case "+" => (a, c) => c - a
    case "*" => (a, c) => c / a
    case "/" => _ / _


  // Left(number): a branch with just monkeys, computed just like part 1 (predictMonkeyResult)
  // Right(number => number): a branch with humn, put in the desired result and get back what humn should have shouted
  def predictHumanInput(who: Monkey, shoutings: Map[Monkey, Shout]): Either[Long, Long => Long] =
    who match
      case "humn" => Right(identity) // return the desired result without change
      case _ =>
        shoutings(who) match
          case Shout.Number(number) => Left(number)
          case Shout.Operation(lhs, rhs, op) =>
            (predictHumanInput(lhs, shoutings), predictHumanInput(rhs, shoutings)) match
              case (Left(x), Left(y)) => Left(executeOp(op)(x, y)) // essentially same as part 1
              case (Right(desired2Humn), Left(y)) => Right(desired => inverseExecuteOpLhs(op)(y, desired).pipe(desired2Humn))
              case (Left(x), Right(desired2Humn)) => Right(desired => inverseExecuteOpRhs(op)(x, desired).pipe(desired2Humn))
              case (Right(_), Right(_)) => ??? // humn can't be on both sides

  val newRoot = input("root").asInstanceOf[Shout.Operation].copy(op = "-") // make root operation 'a - b = 0', supply 0 later as desired result
  val input2 = input + ("root" -> newRoot)

  // part 2
  println(predictHumanInput("root", input2).map(_(0)).merge) // put in 0 as desired outcome, because a=b computed as a-b=0
}

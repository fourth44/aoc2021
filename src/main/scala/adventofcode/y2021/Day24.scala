package adventofcode.y2021

import Parser.*

object D24 {


  @main def Day24: Unit = withResource("day24test.txt") {

    enum Op:
      case Inp, Add, Mul, Div, Mod, Eql

    val anychar = regex("[a-z]".r).map(_.head)

    case class Line(op: Op, dest: Char, arg: Either[Char, Int])

    val lineParser: Parser[Line] =
      (regex("[a-z]{3}".r) ** (whitespace *> anychar <* whitespace) ** (anychar.map(Left(_).withRight[Int]) | num.map(Right(_))).opt.map(_.getOrElse(Right(0)))).map {
        case ((op, dest), arg2) => Line(Op.valueOf(op.capitalize), dest, arg2)
      }

    val instructions = lines().map(lineParser.run(_).toOption.get)

    case class State(mem: Map[Char, Int], input: Seq[Int])

    val getArg: State => Either[Char, Int] => Int = state => _.swap.map(state.mem.apply).merge
    val setArg: State => Char => Int => State = state => c => i => State(state.mem + (c -> i), state.input)

    def step(state: State, line: Line): State = {
      line match {
        case Line(Op.Inp, dest, _) =>
          State(state.mem + (dest -> state.input.head), state.input.drop(1))
        case Line(Op.Add, dest, arg) =>
          setArg(state)(dest)(state.mem(dest) + getArg(state)(arg))
        case Line(Op.Mul, dest, arg) =>
          setArg(state)(dest)(state.mem(dest) * getArg(state)(arg))
        case Line(Op.Div, dest, arg) =>
          setArg(state)(dest)(state.mem(dest) / getArg(state)(arg))
        case Line(Op.Mod, dest, arg) =>
          setArg(state)(dest)(state.mem(dest) % getArg(state)(arg))
        case Line(Op.Eql, dest, arg) =>
          setArg(state)(dest)(if state.mem(dest) == getArg(state)(arg) then 1 else 0)
      }
    }


    val mem0 = Map[Char, Int]('z' -> 22).withDefaultValue(0)

    def run(instr: Seq[Line], inp: Seq[Int], mem: Map[Char, Int]) = instr.foldLeft(State(mem, inp)) { (acc, line) => step(acc, line) }

    println(run(instructions, "99995969919326".map(_.toString.toInt), Map().withDefaultValue(0)))

    val instructionsPerDigit = List.unfold(instructions) {
      case Seq() => None
      case h +: t =>
        val (a, b) = t.span(_.op != Op.Inp)
        Some(h +: a, b)
    }

    val results = for
      (p, i) <- instructionsPerDigit.zipWithIndex
      v <- 1 to 9
    yield
      i -> run(p, Seq(v), mem0).mem('z')


    // 99995969919326
    // 48111514719111

    // results.foreach(println)

    //

  }

}
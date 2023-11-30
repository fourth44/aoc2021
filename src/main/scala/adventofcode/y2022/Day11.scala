package adventofcode.y2022

import scala.math.Integral.Implicits.infixIntegralOps

// LCM without needing GCD, stolen from stackoverflow, but generalized over all numeric types
def lcm[A](list: Seq[A])(using num: Integral[A]): A = list.foldLeft(num.one) { (a, b) =>
  b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
}

@main
def Day11 = withResource("day11a.txt") {
  type MonkeyNum = Int
  
  case class Monkey(num: MonkeyNum, inv: Seq[Long], op: Long => Long, div: Long, ifTrue: MonkeyNum, ifFalse: MonkeyNum, inspected: Long = 0) {
    // to who should the next item go, if any?
    def whatToWho(worryManagement: Long => Long): Option[(MonkeyNum, Long)] = {
      inv.headOption.map { head =>
        val newValue = worryManagement(op(head))
        val newMonkey = if (newValue % div == 0) ifTrue else ifFalse
        newMonkey -> newValue
      }
    }
    def `throw`: Monkey = copy(inv = inv.drop(1), inspected = inspected + 1)
    def accept(item: Long): Monkey = copy(inv = inv :+ item)
  }

  val startMonkeys = lines()
    .grouped(7).map(_.take(6)).toSeq
    .map {
      case Seq(
        s"Monkey ${AsInt(monkey)}:",
        s"  Starting items: ${inv}",
        s"  Operation: new = old ${op} ${amount}",
        s"  Test: divisible by ${AsInt(div)}",
        s"    If true: throw to monkey ${AsInt(trueMonkey)}",
        s"    If false: throw to monkey ${AsInt(falseMonkey)}"
      ) =>
        val binaryOp: (Long, Long) => Long = op match
          case "-" => _ - _
          case "*" => _ * _
          case "+" => _ + _
        val unaryOp: Long => Long = old => amount match
          case "old" => binaryOp(old, old)
          case AsInt(num) => binaryOp(old, num)
        Monkey(monkey, inv.split(",\\s").toSeq.map(_.toLong), unaryOp, div, trueMonkey, falseMonkey)
    }

  extension [A](seq: Seq[A]) def updateWith(index: Int, op: A => A) = seq.updated(index, op(seq(index)))

  extension (monkeys: Seq[Monkey])
    def toss(from: MonkeyNum, to: MonkeyNum, item: Long) =
      monkeys.updateWith(from, _.`throw`).updateWith(to, _.accept(item))

    def playRound(worryManagement: Long => Long): Seq[Monkey] =
      monkeys.indices.foldLeft(monkeys) { (betweenTurns, currentMonkey) =>
        betweenTurns.iterate { betweenTosses => // iterate: continue on 'Left', stop on 'Right'
          val actionOpt = betweenTosses(currentMonkey).whatToWho(worryManagement)
          actionOpt
            .toLeft(right = betweenTosses) // if no items left: stop tossing, return latest state
            .left.map((to, item) => betweenTosses.toss(currentMonkey, to, item)) // otherwise toss an item
        }
      }

  def monkeyBusiness(n: Int, worryManagement: Long => Long) =
    LazyList.iterate(startMonkeys)(_.playRound(worryManagement))(n) // take the nth round
      .map(_.inspected).sorted.reverse.take(2).product

  val factor = lcm(startMonkeys.map(_.div))

  println(monkeyBusiness(20, _ / 3))
  println(monkeyBusiness(10000, _ % factor))
}

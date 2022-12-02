package adventofcode.y2021

@main def Day21: Unit = {
  // Player 1 starting position: 7
  // Player 2 starting position: 10

  // println((1 to 3).sum)

  def rollScore(turn: Int): Seq[Int] =
    val z = (turn * 3)
    Seq(z, z + 1, z + 2).map(x => ((x) % 100) + 1)

/*
  (0 to 3).map(rollScore).foreach(println)
  (28 to 35).map(rollScore).foreach(println)
*/

  // lazy val rolls: Iterator[(Int, Boolean)] = (1 to 100).grouped(3).map(_.sum).zipWithIndex.map { case (sum, i) => (sum, i % 2 == 0) }
  val rolls: Iterator[(Int, Boolean)] =
    Iterator.from(0).map(turn => rollScore(turn).sum -> (turn % 2 == 0))


  def step(prev: Int, amount: Int): Int = ((prev - 1 + amount) % 10) + 1

  val game1 = rolls.scanLeft((7, 0, 10, 0)) { case ((p1, s1, p2, s2), (amount, turn)) =>
    turn match
      case true => // p1
        val next = step(p1, amount)
        (next, s1 + next, p2, s2)
      case false => // p2
        val next = step(p2, amount)
        (p1, s1, next, s2 + next)
  }

  val end = game1.zipWithIndex.find { case ((_, s1, _, s2), i) => s1 >= 1000 || s2 >= 1000 }
  end.foreach(e => println(e._2 * 3 * e._1._4))


  // part 2: quantum dice
  // 21 points win - a player needs at most 7 turns throwing 1's, or 3 turns with a lot of 3's
  // predict at which turn the player wins
  // 222 ternary is 26 decimal
  val probHisto = (0 to 26).map(Integer.toUnsignedString(_, 3).padTo(3, "0")).map(_.map(_.toString.toInt + 1).sum) //.groupBy(identity).view.mapValues(_.size).toMap
  println(probHisto.sorted) // 27

  // je hoeft niet alle 27 combinaties uit te proberen! 3 t/m 9 is genoeg (7 combinaties) en dan vermenigvuldigen met histogram
  // 3, 444, 555555, 6666666, 777777, 888, 9

  // player 1 en 2 onafhankelijk van elkaar! zoek per speler uit hoe snel hij wint met cijfercombinaties (vermenigvuldig met permutaties)
  // daarna die weer tegen elkaar kruiscombineren? cijfercombinaties alleen oplopend maken?
/*
  def choose(n: Int, k: Int): Int =
    if (k == 0 || k == n) 1
    else choose(n - 1, k - 1) + choose(n - 1, k)
*/
  // we need a store?? cursor is 'list of inputs so far'?
  val what = Store[List[Int], Any].apply(list => /* play game */ ???, List())

  val rrr = for {
    one <- 3 to 9
    two <- one to 9
    three <- two to 9
    four <- three to 9
    five <- four to 9
    six <- five to 9
    seven <- six to 9
  } yield {
    List(one, two, three, four, five, six, seven)
  }

  val qqq = rrr.map(nums => nums.scanLeft((8, 0)) { case ((p1, s1), amount) =>
    val next = step(p1, amount)
    (next, s1 + next)
  }.zipWithIndex.find(_._1._2 >= 21).map(x => nums -> x._2).getOrElse(require(false, s"no result yet for ${nums}")))

  // val sss = qqq.groupMap(_._2)(_._1)

  println(qqq)
  // println(Integer.parseUnsignedInt("7777777", 8)) // 2097151
  // val xx = (0 to 2097151).map(Integer.toUnsignedString(_, 8).padTo(7, "0")).map(_.map(_.toString.toInt + 3))


  // each turn, one of 27
  List().permutations
}

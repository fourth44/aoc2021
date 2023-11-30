package adventofcode.y2022

@main
def Day19 = withResource("day19a.txt") {

  enum Mat:
    case Ore, Clay, Obsidian, Geode

  import Mat._

  case class Inventory(robots: Map[Mat, Int], materials: Map[Mat, Int]) {
    def use(build: Mat, from: Map[Mat, Int]): Option[Inventory] = {
      val newMat = from.foldLeft(Option(materials)) { case (accOpt, (reqMat, reqAmount)) => accOpt.flatMap { acc =>
        Option.when(acc.get(reqMat).exists(_ >= reqAmount))(acc + (reqMat -> (acc(reqMat) - reqAmount)))
      }}
      newMat.map(newInv => Inventory(robots + (build -> (robots.getOrElse(build, 0) + 1)), newInv))
    }
    def harvest: Inventory =
      copy(materials = robots.foldLeft(materials) { case (acc, (mat, num)) => acc + (mat -> (acc.getOrElse(mat, 0) + num)) })
  }

  val blueprints: Seq[(Int, Map[Mat, Map[Mat, Int]])] = lines().map {
    case
      s"Blueprint ${AsInt(n)}: Each ore robot costs ${AsInt(m)} ore. Each clay robot costs ${AsInt(o)} ore. Each obsidian robot costs ${AsInt(p)} ore and ${AsInt(q)} clay. Each geode robot costs ${AsInt(r)} ore and ${AsInt(s)} obsidian." =>
      n -> Map(Ore -> Map(Ore -> m), Clay -> Map(Ore -> o), Obsidian -> Map(Ore -> p, Clay -> q), Geode -> Map(Ore -> r, Obsidian -> s))
  }

  val start = Inventory(Map(Ore -> 1), Map())

  def executeBlueprint(num: Int, costs: Map[Mat, Map[Mat, Int]]) = {
    val options = Tree.unfold(start -> 0) { case (inventory, i) =>
      // nee dit zijn alleen stappen die ook echt iets bouwen
      val builds = costs.toSeq.flatMap { case (mat, reqs) =>
        inventory.use(mat, reqs)
      }
      (builds ++ Seq(inventory).filter(_ => builds.isEmpty)).map(inv => inv.harvest -> (i + 1)).filter(_ => i <= 24)
    }
    println(options)
    val after24 = options.fold[Seq[Inventory]] {
      case ((inv, 24), Seq()) => Seq(inv)
      case (_, seqseq) => seqseq.flatten
    }
    println(s"blueprint $num has ${after24.size} results")
    after24.map(_.materials(Geode)).max * num
  }

  println(executeBlueprint(1, blueprints.head._2))

}

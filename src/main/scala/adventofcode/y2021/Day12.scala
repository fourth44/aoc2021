package adventofcode.y2021

enum Cave(val name: String):
  case Big(override val name: String) extends Cave(name)
  case Small(override val name: String) extends Cave(name)

object Cave:
  def apply(name: String): Cave = if name == name.toUpperCase then Cave.Big(name) else Cave.Small(name)

@main def Day12: Unit = withResource("day12a.txt") {

  val tunnels: Map[Cave, Seq[Cave]] = lines()
    .flatMap { case r"([a-zA-Z]+)${from}-([a-zA-Z]+)${to}" =>
      val (f, t) = (Cave(from), Cave(to))
      Seq(f -> t, t -> f)
    }
    .groupMap(_._1)(_._2)
    .view.mapValues(_.filterNot(_.name == "start")).toMap + (Cave("end") -> Seq()) // nobody goes to start; end goes nowhere

  println(tunnels)

  val allPaths = Seq(Seq(Cave("start")) -> Set(Cave("start"))).unfold { case paths =>
    val newPaths = paths.flatMap {
      case (path@(x :+ last), visited) =>
        if last.name == "end" then Seq(path -> visited) else // keep paths to end
          tunnels(last)
            .collect {
              case small: Cave.Small if !visited(small) => (path :+ small) -> (visited + small)
              case big: Cave.Big => (path :+ big) -> visited }
    }
    Either.cond(newPaths.forall(_._1.last.name == "end"), newPaths.map(_._1), newPaths)
  }

  println(allPaths.size)

  // copy-paste with slight modifications:
  val allPaths2 = Seq(Seq(Cave("start")) -> Map[Cave.Small, Int]().withDefaultValue(0)).unfold { paths =>
    val newPaths = paths.flatMap {
      case (path@(x :+ last), visited) =>
        if last.name == "end" then Seq(path -> visited) else // keep paths to end
          tunnels(last)
            .collect {
              case small: Cave.Small if visited(small) == 0 || visited.forall(_._2 < 2) =>
                (path :+ small) -> (visited + (small -> (visited(small) + 1)))
              case big: Cave.Big => (path :+ big) -> visited }
    }
    Either.cond(newPaths.forall(_._1.last.name == "end"), newPaths.map(_._1), newPaths)
  }

  println(allPaths2.size)

}

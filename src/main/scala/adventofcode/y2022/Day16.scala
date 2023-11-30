package adventofcode.y2022
import adventofcode.y2021.aStar

@main
def Day16 = withResource("day16a.txt") {

  val parsed = lines().map {
    case s"Valve ${valve} has flow rate=${AsInt(flowRate)}; tunnel${_} lead${_} to valve${_} ${destinations}" =>
      valve -> (flowRate, destinations.stripPrefix("s").trim.split(",\\s").toSeq)
  }.toMap

  val maxTotalRelease = parsed.map(_._2._1).sum
  val maxRelease = parsed.map(_._2._1).max

  println(maxTotalRelease)
  println(parsed)

  

  // using A* for MAX cost??
  // but how to prevent visiting twice?
  // the DISTANCE must be the SUM OF RELEASED PRESSURE
  // does all this work for negative numbers...?

/*
  val result = aStar[(String, Int, Set[String])]( // where now, how much time left, what already visited
    start        = ("AA", 30, Set()),
    isEnd        = { case (_, time, _) => time <= 0 }, // stop when time is up
    surroundings = { case (x, time, visited) => parsed(x)._2.map(dest => (dest, time - 2, visited + dest))}, // neighbours, but with less time?
    cost         = { case ((from, _, visited), (to, _, _)) => if (!visited(to)) { parsed(from)._1 } else {} },
    h            = { case (from, _, _) => maxTotalRelease - parsed(from._1) }
  )
  println(result)
*/
}

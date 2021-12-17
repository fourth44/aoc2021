package adventofcode.y2021

@main def Day17: Unit = {
  // target area: x=195..238, y=-93..-67
  case class Vector(location: Vect2D[Int], speed: Vect2D[Int])

  val step: Vector => Vector =
    case Vector(Vect2D(x, y), Vect2D(dx, dy)) =>
      Vector(Vect2D(x + dx, y + dy), Vect2D(0 max (dx - 1), dy - 1))

  def bingo(location: Vect2D[Int]): Boolean = location.x >= 195 && location.x <= 238 && location.y >= -93 && location.y <= -67

  // let's just brute force this
  val attempts = for
    x <- 1 to 238
    y <- -93 to 93 // above these values, overshoot is guaranteed
  yield
    Vector(Vect2D(0, 0), Vect2D(x, y))

  val hits = attempts.flatMap { case init =>
    // println(s"${init.speed.x} ${init.speed.y}")
    val positions = LazyList.iterate(init)(step).takeWhile(_.location.y >= -93).map(_.location)
    if positions.exists(bingo) then Some(init -> positions.map(_.y).max) else None
  }

  println(hits.maxBy(_._2)._2) // 4278 at x=20, y=92

  println(hits.size) // 1994





}

package adventofcode.y2021

import math.Numeric.Implicits.infixNumericOps

// at Day2, there was no generic (just Int)
// at Day6, I over-engineered this to be suitable for Doubles if need-be (which it wasn't!)
case class Vect2D[A: Numeric](x: A, y: A):
  def + (other: Vect2D[A]): Vect2D[A] = Vect2D(x + other.x, y + other.y)
  def * (i: A): Vect2D[A] = Vect2D(x * i, y * i)
  def swap: Vect2D[A] = Vect2D(y, x)

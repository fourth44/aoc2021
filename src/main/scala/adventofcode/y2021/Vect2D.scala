package adventofcode.y2021

import math.Numeric.Implicits.infixNumericOps

// at Day2, there was no generic (just Int)
// at Day6, I over-engineered this to be suitable for Doubles if need-be (which it wasn't!)
case class Vect2D[A](x: A, y: A)(using num: Numeric[A]):
  def + (other: Vect2D[A]): Vect2D[A] = Vect2D(x + other.x, y + other.y)
  def * (i: A): Vect2D[A] = Vect2D(x * i, y * i)
  def swap: Vect2D[A] = Vect2D(y, x)
  def seq: Seq[A] = Seq(x, y)
  def map[B: Numeric](f: A => B): Vect2D[B] = Vect2D(f(x), f(y))
  def -(i: Vect2D[A]): Vect2D[A] = this + (i * num.negate(num.one))
  def unit: Vect2D[A] = this.map(_.sign)

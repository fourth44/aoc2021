package adventofcode.y2021

extension [A1, A2, B](f: ((A1, A2)) => B) def untupled: (A1, A2) => B = (a, b) => f((a, b))

class Memo[A,B](private val referentiallyTransparentFunction : A => B) extends (A => B) {
  val memoTable : scala.collection.concurrent.Map[A,B] = scala.collection.concurrent.TrieMap()
  override def apply(v1: A): B = {
    memoTable.getOrElseUpdate(v1, referentiallyTransparentFunction(v1))
  }
}
object Memo {
  def apply[A,B](referentiallyTransparentFunction : A => B) : A => B = new Memo(referentiallyTransparentFunction)
  def apply[A1,A2,B](referentiallyTransparentFunction : (A1, A2) => B) : (A1, A2) => B =
    new Memo(referentiallyTransparentFunction.tupled).untupled
}

object Ints {
  opaque type PosInt = Int
  def mkPos(i: Int): PosInt =
      require(i > 0)
      i

  private val foo = Ordering[Int]
  given Ordering[PosInt] = foo
}

object TestGiven extends App {
  import Ints._
  println(Ordering[PosInt]) // prints null
  // this call gives the correct output somehow?
  println(List(mkPos(4), mkPos(5)).sorted(null))
}

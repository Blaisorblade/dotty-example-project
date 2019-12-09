object Init {
  class Circular[A](val value: A, getter: => Circular[A]) {
    lazy val get: Circular[A] = getter
  }

  def create[A](as: Traversable[A]): Circular[A] = {
    def go[A](as: Traversable[A], initTail: => Circular[A]): Circular[A] = {
      if (as.nonEmpty)
        new Circular(as.head, go(as.tail, initTail))
      else
        initTail
    }
    lazy val b: Circular[A] = go(as, b)
    b
  }

  def prepend[A](a: A, as: Circular[A]): Circular[A] = {
    def go[A](as: Circular[A], origTail: Circular[A], initTail: => Circular[A]): Circular[A] = {
      if (as != origTail)
        new Circular(as.value, go(as.get, origTail, initTail))
      else
        initTail
    }
    lazy val b: Circular[A] = new Circular(a, new Circular(as.value, go(as.get, as, b)))
    b
  }

  def take[A](n: Int)(xs: Circular[A]): List[A] =
    if (n == 0)
      Nil
    else
      xs.value :: take(n - 1)(xs.get)

  def main(xs: Array[String]): Unit = {
    val as1 = create(List(1, 2, 3))
    println(take(15)(as1))
    val as2 = prepend(0, as1)
    println(take(15)(as2))
  }
}

object DoubleInit {
  class DoublyCircular[A](val head: A, nextGetter: => DoublyCircular[A], prevGetter: => DoublyCircular[A]) {
    lazy val next: DoublyCircular[A] = nextGetter
    lazy val prev: DoublyCircular[A] = prevGetter
  }

  def create[A](as: Traversable[A]): DoublyCircular[A] = {

    def go[A](as: Traversable[A],
        initTail: => DoublyCircular[A],
        prev: => Option[DoublyCircular[A]]):
      DoublyCircular[A] =
    {
      if (as.nonEmpty) {
        lazy val n: DoublyCircular[A] =
          new DoublyCircular(as.head, go(as.tail, initTail, Some(n)),
            prev match {case Some(x) => x; case None => ??? })
        n
      }
      else
        initTail
    }
    lazy val b: DoublyCircular[A] = go(as, b, None)
    b
  }
}

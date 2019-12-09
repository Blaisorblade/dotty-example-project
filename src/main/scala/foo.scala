trait Base {
  def foo: Any
}
trait A extends Base {
  override def foo: Int = 1
}
trait B extends Base {
  override def foo: Any = "foo"
}
class C extends A with B {
  override def foo: Int = 2
}


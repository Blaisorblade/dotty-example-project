import scala.language.implicitConversions
object GADTs1 {
  enum Expr[A] {
    case Lit(n: Int) extends Expr[Int]
    case Plus(lhs: Expr[Int], rhs: Expr[Int])
            extends Expr[Int]
    case Var(a: A) extends Expr[A]
    case Fun[A,B](fun: Expr[A] => Expr[B])
            extends Expr[A => B]
    case App[A,B](fun: Expr[A => B], arg: Expr[A])
            extends Expr[B]
  }
  import Expr._
  def eval[A](e: Expr[A]): A = e match {
    case Lit(n) => n
    case Plus(a,b) => eval(a) + eval(b)
    case Var(x) => x
    case f: Fun[a,b] => (x: a) => eval(f.fun(Var(x)))
    case App(fun,arg) => eval(fun)(eval(arg))
  }
}
// class Symantics repr where
//     int :: Int -> repr h Int                -- int literal
//     add :: repr h Int -> repr h Int -> repr h Int

//     z   :: repr (a,h) a                 -- variables: z and s ... (s z)
//     s   :: repr h a -> repr (any,h) a
//     lam :: repr (a,h) b  -> repr h (a->b)
//     app :: repr h (a->b) -> repr h a -> repr h b

object GADTs2 {
  enum Var[G, A] {
    case Z[G, A]() extends Var[(A, G), A]
    case S[G, A, B](x: Var[G, A]) extends Var[(B, G), A]
  }

  def fst[A, B](x: (A, B)): A = x._1
  def snd[A, B](x: (A, B)): B = x._2

  import Var._
  def evalVar[G, A](x: Var[G, A])(rho: G): A = x match {
    case _: Z[g, a] =>
      // rho._1 //fails
      (rho: (a, g))._1 // succeeds
      // fst(rho) // fails
      // fst[a, g](rho) // succeeds
    case s: S[g, a, b] =>
      evalVar(s.x)(snd[b, g](rho))
  }

  enum Expr[G, A] {
    case Lit[G](n: Int) extends Expr[G, Int]
    case V[G, A](x: Var[G, A]) extends Expr[G, A]
    case App[G, A, B](f: Expr[G, A => B], a: Expr[G, A]) extends Expr[G, B]
    case Fun[G, A, B](body: Expr[(A, G), B]) extends Expr[G, A => B]
  }
  import Expr._
  def eval[A, G](e: Expr[G, A])(rho: G): A = e match {
    // case Lit(n) => n // spurious unchecked warning
    case l: Lit[_] => l.n

    case V(x) => evalVar(x)(rho)
    case App(f, a) => eval(f)(rho)(eval(a)(rho))
    case f: Fun[g, a, b] =>
      (x: a) => eval(f.body)(x, rho)
  }
}

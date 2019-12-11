package assert

import scala.quoted._

inline def assert(expr: => Boolean): Unit =
  ${ assertImpl('expr) }

def assertImpl(expr: Expr[Boolean])(given qctx: QuoteContext) = '{
  if (!$expr)
    throw new AssertionError(s"failed assertion: ${${ showExpr(expr) }}")
}

def showExpr(expr: Expr[Boolean])(given qctx: QuoteContext): Expr[String] =
  '{ "<some source code>" } // Better implementation later in this document

import cats.{Applicative, Eval, Functor, Traverse}
import higherkindness.droste._
import higherkindness.droste.data._
import cats.implicits._
import higherkindness.droste.util.DefaultTraverse

object CalculatorNoMacro {
  sealed trait Expr
  object Expr {
    final case class Number(i: Int)                    extends Expr
    final case class Add(left: Expr, right: Expr)      extends Expr
    final case class Multiply(left: Expr, right: Expr) extends Expr

    // and the fixed point version
    sealed trait ExprF[T]
    final case class NumberF[T](i: Int)              extends ExprF[T]
    final case class AddF[T](left: T, right: T)      extends ExprF[T]
    final case class MultiplyF[T](left: T, right: T) extends ExprF[T]

    type λ[α] = ExprF[α]

    implicit val exprfFunctor: Functor[ExprF] = new Functor[ExprF] {
      override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
        case NumberF(i)             => NumberF[B](i)
        case AddF(left, right)      => AddF(f(left), f(right))
        case MultiplyF(left, right) => MultiplyF(f(left), f(right))
      }
    }
    implicit val traverseInstance: Traverse[ExprF] = new DefaultTraverse[ExprF] {
      override def traverse[G[_]: Applicative, AA, B](fa: ExprF[AA])(fn: AA => G[B]): G[ExprF[B]] = fa match {
        case NumberF(i)      => i.pure[G].map(NumberF.apply[B])
        case AddF(l, r)      => (l, r).bitraverse(fn, fn).map(kv => AddF[B](kv._1, kv._2))
        case MultiplyF(l, r) => (l, r).bitraverse(fn, fn).map(kv => MultiplyF[B](kv._1, kv._2))
      }
    }

    def projectCoalgebra: Coalgebra[ExprF, Expr] =
      new GCoalgebra[ExprF, Expr, Expr]({
        case Number(i)      => NumberF[Expr](i)
        case Add(l, r)      => AddF[Expr](l, r)
        case Multiply(l, r) => MultiplyF[Expr](l, r)
      })

    def embedAlgebra: Algebra[ExprF, Expr] =
      new GAlgebra[ExprF, Expr, Expr]({
        case NumberF(i)      => Number(i)
        case AddF(l, r)      => Add(l, r)
        case MultiplyF(l, r) => Multiply(l, r)
      })
    implicit def basisInstance: Basis[ExprF, Expr] = Basis.Default(embedAlgebra, projectCoalgebra)
  }

  import Expr._

  val evaluateAlgebra: Algebra[ExprF, Int] = Algebra[ExprF, Int] {
    case NumberF(i)      => i
    case AddF(l, r)      => l + r
    case MultiplyF(l, r) => l * r
  }

  object parsers {
    import atto._
    import Atto._
    val numberf: Parser[ExprF[String]] = int.map(i => NumberF[String](i))
    val addf: Parser[ExprF[String]] = for {
      l <- takeWhile1(_ != '+')
      _ <- char('+')
      r <- takeRest.map(_.mkString)
    } yield AddF(l, r)
    val multiplyf: Parser[ExprF[String]] = for {
      l <- takeWhile1(_ != '*')
      _ <- char('*')
      r <- takeRest.map(_.mkString)
    } yield MultiplyF(l, r)
    val exprf: Parser[ExprF[String]] =
      List(
        addf,
        multiplyf,
        numberf
      ).reduce(_ | _)
    def apply(s: String): ExprF[String] = exprf.parseOnly(s).either.toOption.get
  }

  val parseAlgebra = Coalgebra[ExprF, String](parsers.apply)

  val evaluate: Expr => Int = scheme.cata(evaluateAlgebra)
  val parse: String => Expr = scheme.ana(parseAlgebra)
  val run: String => Int    = scheme.hylo(evaluateAlgebra, parseAlgebra)

  def main(args: Array[String]): Unit = {
    println(run("2+2"))
  }
}

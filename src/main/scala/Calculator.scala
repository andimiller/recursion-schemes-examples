import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.macros._
import cats.implicits._

object Calculator {
  @deriveFixedPoint sealed trait Expr
  object Expr {
    final case class Number(i: Int) extends Expr
    final case class Add(left: Expr, right: Expr) extends Expr
    final case class Multiply(left: Expr, right: Expr) extends Expr
  }

  import Expr._
  import Expr.fixedpoint._

  val evaluateAlgebra = Algebra[ExprF, Int] {
    case NumberF(i) => i
    case AddF(l, r) => l + r
    case MultiplyF(l, r) => l * r
  }

  object parsers {
    import atto._, Atto._
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
  val run: String => Int = scheme.hylo(evaluateAlgebra, parseAlgebra)

  def main(args: Array[String]): Unit = {
    println(run("2+2"))
  }
}

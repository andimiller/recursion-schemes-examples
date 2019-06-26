import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.macros._
import cats._
import cats.implicits._
import Jason.Expr.IntLiteral
import Jason.Expr.DoubleLiteral
import Jason.Expr.StringLiteral
import io.circe.Json

object Jason {
  type MapOfStringTo[T] = List[(String, T)]

  implicit val mapTraverse: Traverse[MapOfStringTo] = new Traverse[MapOfStringTo] {
    private val trav = Traverse[List]
    override def traverse[G[_], A, B](fa: MapOfStringTo[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[MapOfStringTo[B]] =
      trav.traverse(fa)(t => f(t._2).tupleLeft(t._1) )
    override def foldLeft[A, B](fa: MapOfStringTo[A], b: B)(f: (B, A) => B): B =
      trav.foldLeft(fa, b){ case (bi, (_, a)) => f(bi, a) }
    override def foldRight[A, B](fa: MapOfStringTo[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      trav.foldRight(fa, lb){ case ((_, a), e) => f(a, e) }
  }


  @deriveFixedPoint sealed trait Expr
  object Expr {
    case class IntLiteral(i: Int) extends Expr
    case class DoubleLiteral(d: Double) extends Expr
    case class StringLiteral(s: String) extends Expr
    case class ArrayLiteral(xs: List[Expr]) extends Expr
    case class ObjectLiteral(kv: MapOfStringTo[Expr]) extends Expr
  }

  import Expr._
  import Expr.fixedpoint._

  val evaluateAlgebra = Algebra[ExprF, Json] {
    case IntLiteralF(i) => Json.fromInt(i)
    case DoubleLiteralF(d) => Json.fromDouble(d).get
    case StringLiteralF(s) => Json.fromString(s)
    case ArrayLiteralF(xs) => Json.arr(xs:_*)
    case ObjectLiteralF(kv) => Json.obj(kv:_*)
  }

  object parsers {
    import atto._, Atto._
    import AttoBracketCounting.takeWhileNotUnbracketed

    val intLiteralParser: Parser[ExprF[String]] = int.map(IntLiteralF[String](_))
    val doubleLiteralParser: Parser[ExprF[String]] = double.map(DoubleLiteralF[String](_))
    val stringLiteralParser: Parser[ExprF[String]] = stringLiteral.map(StringLiteralF[String](_))
    val arrayLiteralParser: Parser[ExprF[String]] =
      char('[') *> takeWhileNotUnbracketed(Set(',', ']')).sepBy1(char(',')).map(xs => ArrayLiteralF[String](xs.toList).asInstanceOf[ExprF[String]]) <* char(']')
    val objectLiteralParser: Parser[ExprF[String]] =
      for {
        _ <- char('{')
        bodies <- (
          stringLiteral,
          char(':'),
          takeWhileNotUnbracketed(Set('}', ','))
        ).tupled.map { case (k, _, v) => (k, v) }.sepBy(char(','))
        _ <- char('}')
      } yield ObjectLiteralF[String](bodies)

    def apply(s: String): ExprF[String] = List[Parser[ExprF[String]]](
      objectLiteralParser, arrayLiteralParser, stringLiteralParser, intLiteralParser, doubleLiteralParser
    ).reduce(_ | _).parseOnly(s).either match {
      case Left(t) => throw new Exception(t)
      case Right(r) => r
    }
  }

  val parseAlgebra = Coalgebra[ExprF, String](parsers.apply)
  val evaluate: Expr => Json = scheme.cata(evaluateAlgebra)
  val parse: String => Expr = scheme.ana(parseAlgebra)
  val run: String => Json = scheme.hylo(evaluateAlgebra, parseAlgebra)


}

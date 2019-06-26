import atto._
import Atto._
import cats._, cats.implicits._
import cats.data._

object AttoBracketCounting {

  case class ScanState[T](depth: Int, t: T)

  def takeWhileNotUnbracketed(bads: Set[Char]): Parser[String] = {
    StateT.liftF[Parser, ScanState[String], Unit](().pure[Parser]).whileM_(
      for {
        depth <- StateT.get[Parser, ScanState[String]].map(_.depth)
        body <- StateT.liftF(get)
        continue <- {
          body.headOption match {
            case Some('(') | Some('{') => for {
              _ <- StateT.modify[Parser, ScanState[String]] { case ScanState(d, s) =>
                ScanState(d + 1, s + body.head.toString)
              }
              _ <- StateT.liftF(advance(1))
            } yield true
            case Some(c) if Set(')', '}').contains(c) && bads.contains(c) => for {
              _ <- StateT.modify[Parser, ScanState[String]] { s => s.copy(depth = s.depth - 1)}
            } yield false

            case Some(')') | Some('}') => for {
              _ <- StateT.modify[Parser, ScanState[String]] { case ScanState(d, s) =>
                ScanState(d - 1, s + body.head.toString)
              }
              _ <- StateT.liftF(advance(1))
            } yield true
            case Some(c) if bads.contains(c) && depth == 0 =>
              StateT.liftF[Parser, ScanState[String], Boolean](false.pure[Parser])
            case Some(c) => for {
              _ <- StateT.liftF(advance(1))
              _ <- StateT.modify[Parser, ScanState[String]] { s =>
                s.copy(t = s.t + c.toString)
              }
            } yield true
            case None =>
              StateT.liftF[Parser, ScanState[String], Boolean](false.pure[Parser])
          }
        }
      } yield continue
    ).run(ScanState(0, "")).map(_._1.t)
  }

}

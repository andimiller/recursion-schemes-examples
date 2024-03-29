import atto._
import Atto._
import cats._, cats.implicits._
import cats.data._

object AttoBracketCounting {

  case class ScanState[T](depth: Int, t: T)

  def takeWhileNotUnbracketedInternal(bads: Set[Char],
                                      openBraces: Set[Char] = Set('[', '(', '{'),
                                      closeBraces: Set[Char] = Set(']', ')', '}')): Parser[ScanState[String]] = {
    StateT
      .liftF[Parser, ScanState[String], Unit](().pure[Parser])
      .whileM_(
        for {
          depth <- StateT.get[Parser, ScanState[String]].map(_.depth)
          body  <- StateT.liftF(get)
          continue <- {
            body.headOption match {
              case Some(c) if openBraces.contains(c) =>
                for {
                  _ <- StateT.modify[Parser, ScanState[String]] {
                        case ScanState(d, s) =>
                          ScanState(d + 1, s + c.toString)
                      }
                  _ <- StateT.liftF(advance(1))
                } yield true
              case Some(c) if closeBraces.contains(c) && depth == 0 && bads.contains(c) =>
                StateT.liftF[Parser, ScanState[String], Boolean](false.pure[Parser])
              case Some(c) if closeBraces.contains(c) =>
                for {
                  _ <- StateT.modify[Parser, ScanState[String]] {
                        case ScanState(d, s) =>
                          ScanState(d - 1, s + c.toString)
                      }
                  _ <- StateT.liftF(advance(1))
                } yield true
              case Some(c) if bads.contains(c) && depth == 0 =>
                StateT.liftF[Parser, ScanState[String], Boolean](false.pure[Parser])
              case Some(c) =>
                for {
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
      )
      .run(ScanState(0, ""))
      .map(_._1)
  }

  def takeWhileNotUnbracketed(bads: Set[Char]): Parser[String] = takeWhileNotUnbracketedInternal(bads).map(_.t)
}

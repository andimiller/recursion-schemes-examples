import org.scalatest.{MustMatchers, WordSpec}
import atto._
import Atto._
import AttoBracketCounting._
import cats._, cats.implicits._

class AttoBracketCountingSpec extends WordSpec with MustMatchers {

  "AttoBracketCounting" should {
    val nobad = takeWhileNotUnbracketedInternal(Set.empty)
    "be able to count brackets" in {
      nobad.parseOnly("((").done.either must equal(Right(ScanState(2, "((")))
      nobad.parseOnly("(({{").done.either must equal(Right(ScanState(4, "(({{")))
      nobad.parseOnly("()").done.either must equal(Right(ScanState(0, "()")))
      nobad.parseOnly("((})").done.either must equal(Right(ScanState(0, "((})")))
    }
    val abadandget = (takeWhileNotUnbracketedInternal(Set('a')), get).tupled
    "be able to stop on bad characters when depth is 0" in {
      abadandget.parseOnly("abc").done.either must equal(Right((ScanState(0, ""), "abc")))
      abadandget.parseOnly("()abc").done.either must equal(Right((ScanState(0, "()"), "abc")))
      abadandget.parseOnly("(a)abc").done.either must equal(Right((ScanState(0, "(a)"), "abc")))
    }
    val bracketbadandget = (takeWhileNotUnbracketedInternal(Set('}')), get).tupled
    "be able to stop on bad characters when depth is 0 if the character is a close bracket" in {
      bracketbadandget.parseOnly("{}}").done.either must equal(Right((ScanState(0, "{}"), "}")))
    }
  }

}

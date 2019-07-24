import org.scalatest.{MustMatchers, WordSpec}
import io.circe.Json

class JasonSpec extends WordSpec with MustMatchers {
  "Jason" should {
    "cope with nested stuff" in {
      Jason.run("""[1,2,3,{"a":[1,2],"b":{"hello":"world","1":1}}]""") must equal(
        Json.arr(
          Json.fromInt(1),
          Json.fromInt(2),
          Json.fromInt(3),
          Json.obj(
            "a" -> Json.arr(
              Json.fromInt(1),
              Json.fromInt(2)
            ),
            "b" -> Json.obj(
              "hello" -> Json.fromString("world"),
              "1" -> Json.fromInt(1)
            )
          )
        )
      )
    }
  }
}

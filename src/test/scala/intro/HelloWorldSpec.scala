package intro

import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers


class HelloWorldSpec extends AnyFlatSpec with Matchers {
  "greeting message" should "be nice and friendly" in {
    HelloWorld.message should equal("Hello world!")
  }
}
package typeclass.times
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers

class TimesSpec extends AnyFlatSpec with Matchers {
  import TimesImplicits._

  "times" should "accumulate a List[Int]" in {
    pending
    // 5.times(_ * 10) should equal(List(10, 20, 30, 40, 50))
  }

  it should "accumulate a List[String]" in {
    pending
    // 3.times(_ + "!") should equal(List("1!", "2!", "3!"))
  }
}

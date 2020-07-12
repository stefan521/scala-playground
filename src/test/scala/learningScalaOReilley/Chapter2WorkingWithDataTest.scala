package learningScalaOReilley
import Chapter2WorkingWithData._
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{FlatSpec, Matchers}

class Chapter2WorkingWithDataTest extends FlatSpec with Matchers {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.001)

  it should "formats a string with the given value" in {
    assert(payMeBack(12) == "You owe me $12.0")
    assert(payMeBack(0) == "You don't owe me anything.")
    assert(payMeBack(-22) == "You don't owe me anything.")
    assert(payMeBack(310.2) == "You owe me $310.2")
    assert(payMeBack(5001.22) == "You owe me $5001.22")
  }

  it should "find the phone number in an address with regex" in {
    assert(callFrank("Frank,123 Main,925-555-1943,95122") == "925-555-1943")
  }
}

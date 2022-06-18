package learningscala

import Chapter3ExpressionsAndConditionals._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chapter3ExpressionsAndConditionalsTest extends AnyWordSpec with Matchers {
  "checkName" should {
    "approve valid names" in {
      checkName("Peter") shouldBe "Peter"
      checkName("Maria") shouldBe "Maria"
      checkName("Gibberish") shouldBe "Gibberish"
    }

    "rejects empty and null names" in {
      checkName(null) shouldBe "n/a"
      checkName("") shouldBe "n/a"
    }
  }

  "compareWithZero" should {
    "return 'greater' for positive numbers" in {
      compareWithZero(111.512) shouldBe "greater"
      compareWithZero(0.21) shouldBe "greater"
      compareWithZero(55) shouldBe "greater"
    }

    "return 'less' for negative numbers" in {
      compareWithZero(-0.0002) shouldBe "less"
      compareWithZero(-1030.921) shouldBe "less"
      compareWithZero(-218421) shouldBe "less"
    }

    "return 'same' for 0" in {
      compareWithZero(0) shouldBe "same"
      compareWithZero(0.0) shouldBe "same"
    }
  }

  "convertColor" should {
    "produce hex value of known colours" in {
      convertColor("cyan") shouldBe Some("#00FFFF")
      convertColor("magenta") shouldBe Some("#FF00FF")
      convertColor("yellow") shouldBe Some("#FFFF00")
    }

    "produce None for unknown colours" in {
      convertColor("never-heard-of-it") shouldBe None
    }
  }
}

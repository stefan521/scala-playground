package learningscala

import Chapter5FirstClassFunctions._
import org.scalatest.{Matchers, WordSpec}

class Chapter5FirstClassFunctionsTest extends WordSpec with Matchers {
  "maxInt" should {
    "return the greater Int between the two" in {
      maxInt(-12, 32) shouldBe 32
      maxInt(92, 92) shouldBe 92
      maxInt(451, 21) shouldBe 451
    }
  }

  "multiplyWith" should {
    "return a multiplier function" in {
      val times2 = multiplyWith(2)

      times2(5) shouldBe 10
      times2(90) shouldBe 180
      times2(-1) shouldBe -2

      val times5 = multiplyWith(5)

      times5(3) shouldBe 15
      times5(7) shouldBe 35
      times5(-2) shouldBe -10
    }
  }

  "conditional" should {
    def double(number: Int): Int = number * 2
    def isEven(number: Int): Boolean = number % 2 == 0

    "apply the double function if the argument is even" in {
      conditional[Int](4, isEven, double) shouldBe 8
      conditional[Int](10, isEven, double) shouldBe 20
      conditional[Int](-84, isEven, double) shouldBe -168
    }

    "return the argument if the number is not even" in {
      conditional[Int](5, isEven, double) shouldBe 5
      conditional[Int](-1, isEven, double) shouldBe -1
      conditional[Int](3, isEven, double) shouldBe 3
    }
  }
}

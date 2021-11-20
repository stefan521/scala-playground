package learningscala

import Chapter4Functions._
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

object Chapter4FunctionsTest {
  val randomGenerator = new Random
}
class Chapter4FunctionsTest extends WordSpec with Matchers {
  "computeCircleArea" should {
    "return the circle area given a circle radius" in {
      computeCircleArea(0) shouldBe 0
      computeCircleArea(2) shouldBe 12.56 +- 0.01
      computeCircleArea(10) shouldBe 314.15 +- 0.01
    }
  }

  "parseComputeCircleArea" should {
    import Chapter4FunctionsTest.randomGenerator
    val aRandomDouble = randomGenerator.nextDouble
    val theRandomArea = randomGenerator.nextDouble
    val mockAreaFunction = (_: Double) => theRandomArea

    "return None for empty string" in {
      parseComputeCircleArea("", mockAreaFunction) shouldBe None
    }

    "call the computeCircleArea function after parsing the argument to a double" in {
      parseComputeCircleArea(aRandomDouble.toString, mockAreaFunction) shouldBe Some(theRandomArea)
    }

    "return None if the parsing fails" in {
      parseComputeCircleArea("BIG-CIRCLE!!!!", mockAreaFunction) shouldBe None
    }
  }

  "power" should {
    "raise positive numbers to a power" in {
      power(2, 3) shouldBe 8
      power(3, 3) shouldBe 27
      power(4, 2) shouldBe 16
      power(1, 1) shouldBe 1
    }

    "return 1 when raising something to power of 0" in {
      import Chapter4FunctionsTest.randomGenerator
      power(randomGenerator.nextInt, 0) shouldBe 1
    }

    "handle negative numbers" in {
      power(-2, 2) shouldBe 4
      power(-2, 5) shouldBe -32
    }
  }

  "getManhattanDistance" should {
    "calculate the distance between two points" in {
      getManhattanDistance((15, 21), (15, 21)) shouldBe 0
      getManhattanDistance((15, 21), (18, 12)) shouldBe 12
      getManhattanDistance((15, 21), (-15, 21)) shouldBe 30
    }
  }

  "stringifyTuple" should {
    "make a tuple with the string representation of its members" in {
      class AMockClass {
        override def toString: String = "jumbo-string"
      }
      val aMockClass = new AMockClass

      stringifyTuple("hello", 21.321, aMockClass) shouldBe ("hello", "hello", 21.321, "21.321", aMockClass, "jumbo-string")
    }
  }
}

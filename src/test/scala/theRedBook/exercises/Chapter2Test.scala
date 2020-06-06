package theRedBook.exercises

import java.math.BigInteger

import theRedBook.exercises.Chapter2._

import org.scalatest.{Matchers, WordSpec}

class Chapter2Test extends WordSpec with Matchers {
  "findNthFibonacci" should {
    "find the n-th number in the Fibonacci sequence" in {
      findNthFibonacci(0) shouldBe Some(0)
      findNthFibonacci(1) shouldBe Some(1)
      findNthFibonacci(2) shouldBe Some(3)
      findNthFibonacci(3) shouldBe Some(5)
      findNthFibonacci(4) shouldBe Some(8)
    }

    "not overflow for big numbers that can't be stored in ints or floats" in {
      def toSomeFib(fibAsString: String): Option[BigInt] = {
        Some(new BigInt(new BigInteger(fibAsString)))
      }

      findNthFibonacci(100) shouldBe toSomeFib("927372692193078999176")
      findNthFibonacci(105) shouldBe toSomeFib("10284720757613717413913")
      findNthFibonacci(228) shouldBe toSomeFib("522002106210068326179680117059857997559804836265")
    }

    "return None for negative positions" in {
      findNthFibonacci(-42) shouldBe None
      findNthFibonacci(-231) shouldBe None
      findNthFibonacci(-1) shouldBe None
    }
  }

  "isSorted" should {
    "checks if a List of integers is sorted given a comparison function" in {
      isSorted[Int](List(5, 41, 12, 5), (a: Int, b: Int) => a <= b) shouldBe false
      isSorted[Int](List(5, 5, 12, 41), (a: Int, b: Int) => a <= b) shouldBe true
      isSorted[Int](List(41, 12, 5, 5), (a: Int, b: Int) => a >= b) shouldBe true
      isSorted[Int](List(), (a: Int, b: Int) => a > b) shouldBe true
    }

    "checks if a List of strings is sorted given a comparison function" in {
      def isFirstLexicographicallyLess(lhs: String, rhs: String) = lhs.compare(rhs) < 0

      def isFirstShorter(lhs: String, rhs: String) = lhs.length < rhs.length

      isSorted[String](List("b", "cb", "abc"), isFirstShorter) shouldBe true
      isSorted[String](List("b", "cb", "abc"), isFirstLexicographicallyLess) shouldBe false
      isSorted[String](List("abc", "b", "cb"), isFirstShorter) shouldBe false
      isSorted[String](List("abc", "b", "cb"), isFirstLexicographicallyLess) shouldBe true
      isSorted[String](List(), isFirstLexicographicallyLess) shouldBe true
    }
  }
}

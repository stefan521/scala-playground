package learningScala

import Chapter6Collections._
import org.scalatest.{Matchers, WordSpec}

object Chapter6CollectionsTest {
  case class PalindromesInListTest(
    inputList: List[String],
    expectedResult: (List[String], List[String])
  )

  val palindromesTestsCases = List(
    PalindromesInListTest(
      inputList = List("abc", "vpn"),
      expectedResult = (Nil, List("abc", "vpn"))
    ),
    PalindromesInListTest(
      inputList = Nil,
      expectedResult = (Nil, Nil)
    ),
    PalindromesInListTest(
      inputList = List("a", "racecar"),
      expectedResult = (List("a", "racecar"), Nil)
    ),
    PalindromesInListTest(
      inputList = List("jklkj", "rosu", "mbvvbm", "galben", "semafoare"),
      expectedResult = (List("jklkj", "mbvvbm"), List("rosu", "galben", "semafoare"))
    )
  )
}

class Chapter6CollectionsTest extends WordSpec with Matchers {
  "generateFirstOdds" should {
    "return the first positive odd number" in {
      generateFirstOdds(1) shouldBe List(1)
    }

    "return an empty list for a non-positive number of odds" in {
      generateFirstOdds(0) shouldBe List.empty
      generateFirstOdds(-2) shouldBe List.empty
    }

    "produce a list with the first N positive odd numbers" in {
      generateFirstOdds(3) shouldBe List(1, 3, 5)
      generateFirstOdds(6) shouldBe List(1, 3 ,5 ,7, 9, 11)
    }
  }

  "findFactors" in {
    findFactors(15) shouldBe List(3, 5)
    findFactors(1) shouldBe List()
    findFactors(2) shouldBe List()
    findFactors(0) shouldBe List()
    findFactors(20) shouldBe List(2, 4, 5, 10)
    findFactors(-18) shouldBe List(2, 3, 6, 9)
  }

  "findFactorsInList" in {
    findFactorsInList(List(9, 11, 13, 15)) shouldBe List(3, 3, 5)
    findFactorsInList(List()) shouldBe List()
    findFactorsInList(List(-1, 2, 0, 3, 3)) shouldBe List()
    findFactorsInList(List(3, 3, 2, -2, 9)) shouldBe List(3)
    findFactorsInList(List(10, 20, 27)) shouldBe List(2, 5, 2, 4, 5, 10, 3, 9)
  }

  "getFirstElementsRecursive" in {
    getFirstElementsRecursive(List('a', 't', 'o'), 2) shouldBe List('a', 't')
    getFirstElementsRecursive(List(121, 144, 169, 196, 225), 3) shouldBe List(121, 144, 169)
    getFirstElementsRecursive(List("Vegetable", "Fruit"), 0) shouldBe List()
    getFirstElementsRecursive(List("Vegetable", "Fruit"), 4) shouldBe List("Vegetable", "Fruit")
    getFirstElementsRecursive(List(3.14, 6.22, 9.5), -7) shouldBe List()
  }

  "findLongestString" in {
    findLongestString(List("house", "mouse", "winter vegetables")) shouldBe "winter vegetables"
    findLongestString(List()) shouldBe ""
    findLongestString(List("house", "mouse")) shouldBe "house"
    findLongestString(List("solo")) shouldBe "solo"
  }

  "reverseList" should {
    "return the same list if it has less than 2 elements" in {
      reverseList(List()) shouldBe List()
      reverseList(List(152)) shouldBe List(152)
    }

    "return the reversed list" in {
      reverseList(List('a', 'b', 'c')) shouldBe  List('c', 'b', 'a')
    }
  }

  "findPalindromesWithPartition" should {
    import Chapter6CollectionsTest.palindromesTestsCases

    "return a tuple separating palindromes from non-palindromes" in {
      for (testCase <- palindromesTestsCases) {
        findPalindromesWithPartition(testCase.inputList) shouldBe testCase.expectedResult
      }
    }
  }

  "findPalindromesWithRecursion" should {
    import Chapter6CollectionsTest.palindromesTestsCases

    "return a tuple separating palindromes from non-palindromes" in {
      for (testCase <- palindromesTestsCases) {
        findPalindromesWithRecursion(testCase.inputList) shouldBe testCase.expectedResult
      }
    }
  }
}

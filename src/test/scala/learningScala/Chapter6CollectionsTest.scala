package learningScala

import helper.TestCase
import Chapter6Collections._
import org.scalatest.FunSpec

class Chapter6CollectionsTest extends FunSpec {
  it("generate first odd numbers starting with 1") {
    assert(generateFirstOdds(1) == List(1))
    assert(generateFirstOdds(0) == List())
    assert(generateFirstOdds(-2) == List())
    assert(generateFirstOdds(3) == List(1, 3, 5))
    assert(generateFirstOdds(6) == List(1, 3 ,5 ,7, 9, 11))
  }

  it("find factors of a number") {
    assert(findFactors(15) == List(3, 5))
    assert(findFactors(1) == List())
    assert(findFactors(2) == List())
    assert(findFactors(0) == List())
    assert(findFactors(20) == List(2, 4, 5, 10))
    assert(findFactors(-18) == List(2, 3, 6, 9))
  }

  it("find factors in a list") {
    assert(findFactorsInList(List(9, 11, 13, 15)) == List(3, 3, 5))
    assert(findFactorsInList(List()) == List())
    assert(findFactorsInList(List(-1, 2, 0, 3, 3)) == List())
    assert(findFactorsInList(List(3, 3, 2, -2, 9)) == List(3))
    assert(findFactorsInList(List(10, 20, 27)) == List(2, 5, 2, 4, 5, 10, 3, 9))
  }

  it("take first elements of a list") {
    assert(getFirstElementsRecursive(List('a', 't', 'o'), 2) == List('a', 't'))
    assert(getFirstElementsRecursive(List(121, 144, 169, 196, 225), 3) == List(121, 144, 169))
    assert(getFirstElementsRecursive(List("Vegetable", "Fruit"), 0) == List())
    assert(getFirstElementsRecursive(List("Vegetable", "Fruit"), 4) == List("Vegetable", "Fruit"))
    assert(getFirstElementsRecursive(List(3.14, 6.22, 9.5), -7) == List())
  }

  it("find longest string in a list") {
    assert(findLongestString(List("house", "mouse", "winter vegetables")) == "winter vegetables")
    assert(findLongestString(List()) == "")
    assert(findLongestString(List("house", "mouse")) == "house")
    assert(findLongestString(List("solo")) == "solo")
  }

  it("reverse a list") {
    assert(reverseList(List()) == List())
    assert(reverseList(List(152)) == List(152))
    assert(reverseList(List('a', 'b', 'c')) == List('c', 'b', 'a'))
  }

  describe("find the palindromes in a list") {
    val inputs: List[TestCase[List[String], (List[String], List[String])]] = List(
      TestCase(input = Nil, output = (Nil, Nil)),
      TestCase(input = List("abc", "vpn"), output = (Nil, List("abc", "vpn"))),
      TestCase(input = List("a", "racecar"), output = (List("a", "racecar"), Nil)),
      TestCase(input = List("jklkj", "rosu", "mbvvbm", "galben", "semafoare"),
        output = (List("jklkj", "mbvvbm"), List("rosu", "galben", "semafoare")))
    )

    it("with partition") {
      TestCase.assertTestCases(inputs, findPalindromesWithPartition)
    }

    it("with recursion") {
      TestCase.assertTestCases(inputs, findPalindromesWithRecursion)
    }
  }
}

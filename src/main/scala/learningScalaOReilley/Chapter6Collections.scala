package learningScalaOReilley

object Chapter6Collections {
  def generateFirstOdds(oddCount: Int): List[Int] = {
    val lastOdd = oddCount * 2 - 1
    (1 to lastOdd by 2).toList
  }

  def findFactors(num: Int): List[Int] = (2 until math.abs(num)).filter(num % _ == 0).toList

  def findFactorsInList(numbers: List[Int]): List[Int] = numbers.flatMap(findFactors)

  def getFirstElementsRecursive[A](items: List[A], count: Int): List[A] = {
    @scala.annotation.tailrec
    def getFirstElementsTailRec(items: List[A], itemsToTake: List[A], count: Int): List[A] = items match {
      case x :: xs if count > 0 => getFirstElementsTailRec(xs, itemsToTake:::List(x), count - 1)
      case _ => itemsToTake
    }

    getFirstElementsTailRec(items, Nil, count)
  }

  def findLongestString(strings: List[String]): String = strings match {
    case Nil => ""
    case stringList => stringList.reduce((lhs, rhs) => {
      if(lhs.length >= rhs.length) lhs else rhs
    })
  }

  def reverseList[A](fromList: List[A]): List[A] = fromList match {
    case Nil => Nil
    case x::xs => reverseList(xs):::List(x)
  }

  def isPalindrome(s: String): Boolean = s == s.reverse

  def findPalindromesWithPartition(lst: List[String]): (List[String], List[String]) = {
    lst.partition(isPalindrome)
  }

  def findPalindromesWithRecursion(lst: List[String]): (List[String], List[String]) = {
    @scala.annotation.tailrec
    def doPartition(lst: List[String], palindromes: List[String], notPalindromes: List[String]): (List[String], List[String]) = lst match {
      case Nil => (palindromes.reverse, notPalindromes.reverse)
      case x::xs if isPalindrome(x) => doPartition(xs, x::palindromes, notPalindromes)
      case x::xs if !isPalindrome(x) => doPartition(xs, palindromes, x::notPalindromes)
    }

    doPartition(lst, Nil, Nil)
  }
}

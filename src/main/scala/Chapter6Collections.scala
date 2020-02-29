object Chapter6Collections {
  def generateFirstOdds(oddCount: Int): List[Int] = {
    val lastOdd = oddCount * 2 - 1
    (1 to lastOdd by 2).toList
  }

  def findFactors(num: Int): List[Int] = (2 until math.abs(num)).filter(num % _ == 0).toList

  def findFactorsInList(numbers: List[Int]): List[Int] = numbers.map(findFactors).flatten

  def first[A](items: List[A], count: Int): List[A] = {
    if(count  < 1 || items.isEmpty) Nil
    else items match {
      case Nil => Nil
      case x::xs => x::first(xs, count - 1)
    }
  }

  def findLongestString(strings: List[String]): String = {
    if(strings.isEmpty) ""
    else strings.reduce((lhs, rhs) => {
      if(lhs.size >= rhs.size) lhs else rhs
    })
  }

  def reverseList[A](fromList: List[A]): List[A] = {
    fromList match {
      case Nil => Nil
      case x::xs => reverseList(xs):::List(x)
    }
  }

  def isPalindrome(s: String): Boolean = s == s.reverse

  def findPalindromesWithPartition(lst: List[String]): (List[String], List[String]) = {
    lst.partition(isPalindrome)
  }

  def findPalindromesWithRecursion(lst: List[String]): (List[String], List[String]) = {
    @scala.annotation.tailrec
    def doPartition(lst: List[String], palindromes: List[String], notPalindromes: List[String]): (List[String], List[String]) = {
      lst match {
        case Nil => (palindromes.reverse, notPalindromes.reverse)
        case x::xs if isPalindrome(x) => doPartition(xs, x::palindromes, notPalindromes)
        case x::xs if !isPalindrome(x) => doPartition(xs, palindromes, x::notPalindromes)
      }
    }

    doPartition(lst, Nil, Nil)
  }
}

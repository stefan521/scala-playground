package theRedBook.exercises

object Chapter2 extends App {
  /**
   *
   * @param n The position in the Fibonacci sequence 0-based.
   * @return The n-th Fibonacci number.
   */
  def findNthFibonacci(n: Int): Option[BigInt] = {
    @scala.annotation.tailrec
    def findFib(pos: Int, oneFibBehind: BigInt, twoFibsBehind: BigInt): BigInt = {
      val nextFib = twoFibsBehind + oneFibBehind

      if (pos < n) findFib(pos + 1, nextFib, oneFibBehind)
      else nextFib
    }

    if (n == 0 || n == 1) Some(n)
    else if (n < 0) None
    else Some(findFib(pos = 0, oneFibBehind = 1, twoFibsBehind = 0))
  }

  /**
   *
   * @param arr The Sequence to check
   * @param ordered Determines if two consecutive elements are in order
   * @tparam A Type of elements in the Sequence
   * @return true if the sequence is sorted according to the ordered comparison function
   */
  def isSorted[A](arr: Seq[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def checkSorted(head: A, seq: Seq[A], sorted: Boolean = true): Boolean = seq match {
      case Nil =>
        sorted
      case x::xs =>
        val stillSorted = sorted && ordered(head, x)
        checkSorted(x, xs, stillSorted)
    }

    if (arr.size < 2) true
    else checkSorted(arr.head, arr.tail)
  }
}

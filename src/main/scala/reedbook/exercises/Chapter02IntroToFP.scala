package reedbook.exercises

object Chapter02IntroToFP extends App {
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

  /**
   *
   * @param f the function to be curried
   * @tparam A type of the first argument of f
   * @tparam B type of the second argument of f
   * @tparam C return type of f
   * @return the curried function f
   */
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
   *
   * @param f the function to be uncurried
   * @tparam A type of the first argument of f
   * @tparam B type of the second argument of f
   * @tparam C return type of f
   * @return the uncurried function f
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   *
   * @param g The function to be applied first
   * @param f The function to be applied second
   * @tparam A Type of argument of g
   * @tparam B Type of argument of f
   * @tparam C return type of f
   * @return The result of applying g and then f on an argument of type A
   */
  def compose[A, B, C](g: A => B, f: B => C): A => C = {
    (a: A) => f(g(a))
  }
}

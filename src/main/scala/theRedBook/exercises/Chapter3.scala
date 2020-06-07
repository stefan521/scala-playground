package theRedBook.exercises

object Chapter3 {
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case _::xs => xs
  }

  def setHead[A](lst: List[A], value: A): List[A] = lst match {
    case Nil => List(value)
    case _::xs => value::xs
  }

  def drop[A](lst: List[A], n: Int): List[A] = {
    @scala.annotation.tailrec
    def callTail(tailList: List[A], callsCount: Int): List[A] = {
      if (callsCount >= n) tailList
      else callTail(tail(lst), callsCount + 1)
    }
    
    callTail(lst, callsCount = 0)
  }

  def dropWhile[A](lst: List[A])(f: A => Boolean): List[A] = {
    @scala.annotation.tailrec
    def callTailWhile(tailList: List[A]): List[A] = {
      if (f(tailList.head)) callTailWhile(tail(tailList))
      else tailList
    }

    callTailWhile(lst)
  }

  def init[A](lst: List[A]): List[A] = {
    @scala.annotation.tailrec
    def takeAllButLastElement(fromList: List[A], toList: List[A]): List[A] = fromList match {
      case Nil => toList.reverse
      case x::xs => takeAllButLastElement(xs, x::toList)
    }

    takeAllButLastElement(lst, List.empty)
  }

  def lengthOfListUsingFoldRight[A](lst: List[A]): Int = {
    @scala.annotation.tailrec
    def computeLength(list: List[A], length: Int = 0): Int = list match {
      case Nil => length
      case _::xs => computeLength(xs, length + 1)
    }

    computeLength(lst)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def fold(acc: B, list: List[A]): B = list match {
      case Nil => acc
      case x::xs => fold(f(acc, x), xs)
    }

    fold(z, as)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as.reverse, z)(f)

  def sumWithFoldLeft(list: List[Int]): Int = foldRight(list, 0)(_ + _)

  def productWithFoldLeft(list: List[Int]): Int = foldRight(list, 1)(_ * _)

  def lengthWithFoldLeft[A](list: List[A]): Int = foldRight(list, 0)((size, _) => size + 1)

  def appendListWithFold[A](fromList: List[A], toList: List[A]): List[A] = {
    foldRight(fromList.reverse, toList)((accList, newHead) => newHead::accList)
  }

  def reverseListWithFold[A](list: List[A]): List[A] = {
    foldRight(list, List[A]())((reversedList: List[A], newHead: A) => newHead::reversedList)
  }

  def flattenList[A](list: List[List[A]]): List[A] = {
    foldRight(list.reverse, List[A]())((flatList, nextList) => appendListWithFold(nextList, flatList))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as.reverse, List[B]())((mappedList, newHead) => f(newHead)::mappedList)
  }

  def listElementsToString[A](list: List[A]): List[String] = map(list)(_.toString)

  def listElementsPlusOne(list: List[Int]): List[Int] = map(list)(_ + 1)

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as.reverse, List[A]())((filteredList, element) => {
      if (f(element)) element::filteredList
      else filteredList
    })
  }

}

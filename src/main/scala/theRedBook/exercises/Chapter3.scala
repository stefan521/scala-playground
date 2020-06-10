package theRedBook.exercises

import scala.math.max

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

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    val mappedList = map(as)(f)

    flattenList(mappedList)
  }

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(element => if (f(element)) List(element) else Nil)
  }

  def addListElementsTogether(list1: List[Int], list2: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def combine(lhs: List[Int], rhs: List[Int], resultingList: List[Int]): List[Int] = (lhs, rhs) match {
      case (Nil, _) =>
        appendListWithFold(resultingList.reverse, rhs)
      case (_, Nil) =>
        appendListWithFold(resultingList.reverse, lhs)
      case (x::xs, y::ys) =>
        combine(xs, ys, (x + y)::resultingList)
    }

    combine(list1, list2, Nil)
  }

  // generalization of the function above
  def zipListsWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[Any] = {
    @scala.annotation.tailrec
    def combine(lhs: List[A], rhs: List[B], resultingList: List[C]): List[Any] = (lhs, rhs) match {
      case (Nil, _) =>
        appendListWithFold(resultingList.reverse, rhs)
      case (_, Nil) =>
        appendListWithFold(resultingList.reverse, lhs)
      case (x::xs, y::ys) =>
        combine(xs, ys, f(x, y)::resultingList)
    }

    combine(list1, list2, Nil)
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def treeSize[A](tree: Tree[A]): Int = {
    @scala.annotation.tailrec
    def countNodes(tree: Tree[A], count: Int, branchesToExplore: List[Tree[A]]): Int = tree match {
      case _: Leaf[A]  =>
        if (branchesToExplore.isEmpty) count + 1
        else countNodes(branchesToExplore.head, count + 1, branchesToExplore.tail)

      case branch: Branch[A] =>
        countNodes(branch.left, count + 1, branch.right::branchesToExplore)
    }

    countNodes(tree, count= 0, List.empty)
  }

  def maximumElement(tree: Tree[Int]): Int = {
    @scala.annotation.tailrec
    def findMax(tree: Tree[Int], maximum: Int, branchesToExplore: List[Tree[Int]]): Int = tree match {
      case leaf: Leaf[Int]  =>
        val newMax = max(leaf.value, maximum)

        if (branchesToExplore.isEmpty) newMax
        else findMax(branchesToExplore.head, newMax, branchesToExplore.tail)

      case branch: Branch[Int] =>
          findMax(branch.left, maximum, branch.right::branchesToExplore)
    }

    findMax(tree, Int.MinValue, List.empty)
  }

  def depth[A](tree: Tree[A]): Int = {
    @scala.annotation.tailrec
    def findMaxDepth(t: Tree[A], currentDepth: Int, maxDepth: Int, branchesToExplore: List[Tree[A]]): Int = t match {
      case _: Leaf[A] =>
        max(currentDepth + 1, maxDepth)
      case branch: Branch[A] =>
        findMaxDepth(branch.left, currentDepth + 1, maxDepth, branch.right::branchesToExplore)
    }

    findMaxDepth(tree, currentDepth= 0, maxDepth= 0, List.empty)
  }
}

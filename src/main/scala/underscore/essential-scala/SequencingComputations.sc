import scala.annotation.tailrec

sealed trait LinkedList[A] {

  def length(): Long = {
    @tailrec
    def doLength(lengthSoFar: Long, list: LinkedList[A]): Long =
      list match {
        case End() => lengthSoFar
        case Pair(_, tail) => doLength(1L + lengthSoFar, tail)
      }

    doLength(0, this)
  }

  def contains(value: A): Boolean = {
    @tailrec
    def doContains(list: LinkedList[A]): Boolean =
      list match {
        case End() => false
        case Pair(head, tail) if head == value => true
        case Pair(_, tail) => doContains(tail)
      }

    doContains(this)
  }

  def apply(elementNumber: Long): Option[A] = {
    @tailrec
    def doApply(list: LinkedList[A], position: Long): Option[A] =
      list match {
        case End() =>
          None

        case Pair(head, _) if position == elementNumber =>
          Some(head)

        case Pair(_, tail) =>
          doApply(tail, position + 1)

      }

    doApply(this, 0)
  }

  def fold[B](end: B)(f: (A, B) => B): B =
    this match {
      case End() => end
      case Pair(hd, tl) => f(hd, tl.fold(end)(f))
    }

}

final case class End[A]() extends LinkedList[A]

final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

sealed trait IntList extends LinkedList[Int] {
  def sum: Int = this.fold(0)(_ + _)

  def product: Int = this.fold(1)(_ * _)

  override def length: Long = this.fold(0)((_, count) => count + 1)
}



sealed trait Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B
}

final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def fold[B](node: (B, B) => B, leaf: A => B): B =
    node(left.fold(node, leaf), right.fold(node, leaf))
}

final case class Leaf[A](value: A) extends Tree[A] {
  override def fold[B](node: (B, B) => B, leaf: A => B): B =
    leaf(value)
}

val tree: Tree[String] =
  Node(
    Node(
      Leaf("To"),
      Leaf("iterate")
    ),
    Node(
      Node(
        Leaf("is"),
        Leaf("human,")
      ),
      Node(
        Leaf("to"),
        Node(
          Leaf("recurse"),
          Leaf("divine")
        )
      )
    )
  )

tree.fold[String]((left, right) => s"$left $right", identity)

case class Pair[A, B](one: A, two: B)

// it's like Either
sealed trait Sum[A, B] {
  def fold[C](f: A => C, g: B => C): C =
    this match {
      case LeftSum(value) => f(value)
      case RightSum(value) => g(value)
    }

  def flatMap[C](f: B => Sum[A, C]): Sum[A, C] =
    this match {
      case RightSum(value) => f(value)
      case LeftSum(value)  => LeftSum(value)

    }

  def map[C](f: B => C): Sum[A, C] =
    flatMap(v => RightSum(f(v)))
}

final case class LeftSum[A, B](value: A) extends Sum[A, B]

final case class RightSum[A, B](value: B) extends Sum[A, B]


// it's like Option
// covariance allows us to omit the type parameter for Empty, because everything extends Nothing
sealed trait Maybe[+A] {
  def fold[B](f: A => B, empty: B): B =
    this match {
      case Full(value) => f(value)
      case Empty => empty
    }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] =
    this match {
      case Full(value) => f(value)
      case Empty => Empty
    }

  def map[B](f: A => B): Maybe[B] =
    this.flatMap(v => Full(f(v)))
}

final case class Full[+A](value: A) extends Maybe[A]

final case object Empty extends Maybe[Nothing]

val aMaybe: Maybe[Boolean] = Empty



val list = List(1, 2, 3)

list.flatMap(el => List(el, -el))

def isEven(num: Int): Boolean = num % 2 == 0

val list: List[Maybe[Int]] = List(Full(3), Full(2), Full(1))

list.map {
  case el @ Full(value) if isEven(value) => el
  case _ => Empty
}
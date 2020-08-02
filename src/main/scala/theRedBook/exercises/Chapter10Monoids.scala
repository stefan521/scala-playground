package theRedBook.exercises

import scala.annotation.tailrec

object Chapter10Monoids extends App {
  trait Monoid[A] {
    def  op(a: A, b: A): A // must be associative (parens don't matter)
    def zero: A // op(zero, a) must always give a same for op(a, zero)
  }

  val stringMonoid = new Monoid[String] {
    override def op(a: String, b: String): String = a + b // concatenation

    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a: List[A], b: List[A]): List[A] = a ++ b

    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a || b

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a && b

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)

    override def zero: Option[A] = None
  }

  // a monoid for endofunctions (the domain is the same as the codomain
  def endoMonoid[A]: Monoid[A => A] =  new Monoid[A => A]{
    override def op(a: A => A, b: A => A): A => A =
      (finallyAnArgument: A) => b(a(finallyAnArgument))

    override def zero: A => A = identity[A]
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def foldLeftWithFoldMap[A](as: List[A], m: Monoid[A]): A =
    foldMap[A, A](as, m)(identity)

  def foldRightWithFoldMap[A](as: List[A], m: Monoid[A]): A =
    foldMap[A, A](as.reverse, m)(identity)

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    def go(toProcess: IndexedSeq[A]): B = {
      toProcess match {
        case x if x.isEmpty =>
          m.zero

        case IndexedSeq(x) =>
          f(x)

        case seq =>
          val (left, right) = seq.splitAt(seq.length / 2)
          val processedLeft = go(left)
          val processedRight = go(right)
          m.op(processedLeft, processedRight)  // is not tail-recursive... :(  YET
      }
    }

    go(v)
  }

  object FunnyWayToCheckOrdering {
    type IntAndIndex = (Int, Int)

    val orderingMonoid: Monoid[IntAndIndex] = new Monoid[IntAndIndex] {
      override def op(a: IntAndIndex, b: IntAndIndex): IntAndIndex = if (a._1 < b._1) a else b

      override def zero: IntAndIndex = (Int.MaxValue, Int.MinValue)
    }

    def isOrderedWithFoldMap(v: IndexedSeq[Int]): Boolean = {
      val minIndex = foldMap(v.toList.zipWithIndex, orderingMonoid)(identity)

      v.isEmpty || minIndex._2 == 0
    }
  }

  object WordCountingInChunks {
    sealed trait WC
    case class Stub(chars: String) extends WC
    case class Part(lStub: String, words: Int, rStub: String) extends WC

    def countWordsInStr(str: String): Int = str.split("").length

    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      override def op(a: WC, b: WC): WC = (a, b) match {
        case (aStub: Stub, bStub: Stub) =>
          Stub(aStub.chars + bStub.chars)

        case (stub: Stub, part: Part) =>
          Part(stub.chars + part.lStub, part.words, part.rStub)

        case (part: Part, stub: Stub) =>
          Part(part.lStub, part.words, part.rStub + stub.chars)

        case (Part(lStubA, wordsA, rStubA), Part(lStubB, wordsB, rStubB)) =>
          val countedWords = wordsA + wordsB
          val totalWords = if ((rStubA + lStubB).isBlank) countedWords else countedWords + 1
          Part(lStubA, totalWords, rStubB)
      }

      override def zero: WC = Stub("")
    }
  }
}

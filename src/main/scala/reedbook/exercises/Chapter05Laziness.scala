package reedbook.exercises

import reedbook.exercises.Chapter05Laziness.Stream.{cons, empty}

import scala.annotation.tailrec

object Chapter05Laziness extends App {

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _ => empty
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    @tailrec
    final def forAll(predicate: A => Boolean): Boolean = this match {
      case Empty => true
      case Cons(h, t) => predicate(h()) && t().forAll(predicate)
    }

    // If f does not use the second parameter the execution terminates early (recursion does not happen...)
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def takeWhileWithFoldRight(predicate: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (el, acc) => if (predicate(el)) cons(el, acc) else empty }

    def headOptionWithFoldRight: Option[A] =
      foldRight(Option.empty[A]) { (h, _) => Some(h) }

    def map[B](fn: A => B): Stream[B] =
      foldRight(empty[B]) { (el, acc) => cons(fn(el), acc) }

    def filter(fn: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (el, acc) => if (fn(el)) cons(el, acc) else acc }

    def append[A2>:A](that: Stream[A2]): Stream[A2] =
      foldRight(that) { (el, acc) => cons(el, acc) }

    def flatMap[B](fn: A => Stream[B]): Stream[B] =
      foldRight(empty[B]) { (el, acc) => fn(el).append(acc) }

    def mapWithUnfold[B](fn: A => B): Stream[B] =
      Stream.unfold(this) {
        case Empty => None
        case Cons(h, t) => Some(fn(h()), t())
      }

    def takWithUnfold(n: Int): Stream[A] =
      Stream.unfold(this) {
        case Cons(h, t) if n > 0 => Some(h(), t().takWithUnfold(n - 1))
        case _ => None
      }

    def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
      Stream.unfold(this) {
        case Cons(h, t) if p(h()) => Some(h(), t().takeWhileWithUnfold(p))
        case _ => None
      }

  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _ *))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def doFibs(a: Int, b: Int): Stream[Int] = {
        val nextFib = a + b
        cons(nextFib, doFibs(b, nextFib))
      }

      doFibs(0, 1)
    }


    def unfold[A, S](state: S)(f: S => Option[(A, S)]): Stream[A] =
      f(state) match {
        case Some((el, nextState)) => cons(el, unfold(nextState)(f))
        case None => empty[A]
      }

    def fibsWithUnfold: Stream[Int] =
      unfold((0, 1)) { case (currentFib1, currentFib2) =>
        val nextFib = currentFib1 + currentFib2
        Some(nextFib, (currentFib2, nextFib))
      }

    def fromWithUnfold(n: Int): Stream[Int] = unfold(n) { x => Some(x, x + 1) }

    def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

    def onesWithUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  }

  println(Stream.constant(5).take(10).toList)
}

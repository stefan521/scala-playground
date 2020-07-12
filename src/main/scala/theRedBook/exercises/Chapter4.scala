package theRedBook.exercises

import scala.annotation.tailrec

object Chapter4 extends App {

  object MyOption {
    def map2[A, B, C] (a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
      for {
        aVal <- a
        bVal <- b
      } yield f(aVal, bVal)

    def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = {
      traverse(a)(identity)
    }

    def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
      @scala.annotation.tailrec
      def combineOptions(optionList: List[A], combined: List[B]): MyOption[List[B]] = optionList match {
        case List.empty =>
          MySome(combined)

        case x::xs =>
          val transformed = f(x)

          if (transformed.isDefined)
            combineOptions(xs, transformed::combined)
          else
            MyNone
      }

      combineOptions(a, List.empty)
    }
  }

  sealed trait MyOption[+A] {
    def map[B] (f: A => B): MyOption[B] = this match {
      case MyNone => MyNone
      case some: MySome[A] => MySome(f(some.get()))
    }

    def flatMap[B] (f: A => MyOption[B]): MyOption[B] = this match {
      case MyNone => MyNone
      case some: MySome[A] => f(some.get())
    }

    def getOrElse[B >: A] (default: => B): B = this match {
      case MyNone => default
      case some: MySome[A] => some.get()
    }

    def orElse[B >: A] (ob: => MyOption[B]): MyOption[B] = this match {
      case MyNone => ob
      case some: MySome[A] => some
    }

    def filter(f: A => Boolean): MyOption[A] = this match {
      case MyNone => MyNone
      case some: MySome[A] => if (f(some.get())) some else MyNone
    }

    def isDefined: Boolean = this match {
      case MyNone => false
      case _ => true
    }

    def get(): A
  }

  case class MySome[+A] (value: A) extends MyOption[A] {
    override def get(): A = value
  }

  case object MyNone extends MyOption[Nothing] {
    override def get() = throw new NoSuchMethodException("None.get")
  }

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case sequence => Some(sequence.sum / sequence.length)
  }

  // if the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each x in the sequence
  def variance(xs: Seq[Double]): Option[Double] = {
    def mapXsWithMean(m: Double): Seq[Double] = xs.map(x => math.pow(x - m, 2))

    mean(xs).flatMap(m => mean(mapXsWithMean(m)))
  }



  // Either
  object Either {
    def sequence[E, A] (es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

    def traverse[E, A, B] (as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      @tailrec
      def go(lst: List[A], result: List[B]): Either[E, List[B]] = lst match {
        case List.empty =>
          Right(result)

        case x::xs =>
          val tryNext = f(x)

          tryNext match {
            case Right(_) =>
              go(xs, tryNext :: result)

            case Left(err) =>
              Left(err)
          }
      }

      go(as, List.empty)
    }
  }

  sealed trait Either[+E, +A] {
    def map [B] (f: A => B): Either[E, B] = this match {
      case Right(value) => Right(f(value))
      case Left(value) => Left(value)
    }

    def flatMap[EE >: E, B] (f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => f(value)
      case Left(value) => Left(value)
    }

    def orElse[EE >: E, B >: A] (b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => this
      case Left(_) => b
    }

    def map2[EE >: E, B, C] (b: Either[EE, B]) (f: (A, B) => C): Either[EE, C] =
      for {
        aVal <- this
        bVal <- b
      } yield f(aVal, bVal)

    def isRight: Boolean = this match {
      case Right(_) => true
      case _ => false
    }

    def isLeft: Boolean = this match {
      case Left(_) => true
      case _ => false
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]
}

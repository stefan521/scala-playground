package theRedBook.exercises

object Chapter4 extends App {
  sealed trait MyOption[+A] {
    def map[B] (f: A => B): MyOption[B] = this match {
      case MyNone => MyNone
      case some: MySome[A] => MySome(f(some.get))
    }

    def flatMap[B] (f: A => MyOption[B]): MyOption[B] = this match {
      case MyNone => MyNone
      case some: MySome[A] => f(some.get)
    }

    def getOrElse[B >: A] (default: => B): B = this match {
      case MyNone => default
      case some: MySome[A] => some.get
    }

    def orElse[B >: A] (ob: => MyOption[B]): MyOption[B] = this match {
      case MyNone => ob
      case some: MySome[A] => some
    }

    def filter(f: A => Boolean): MyOption[A] = this match {
      case MyNone => MyNone
      case some: MySome[A] => if (f(some.get)) some else MyNone
    }
  }

  case class MySome[+A] (get: A) extends MyOption[A]
  case object MyNone extends MyOption[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case sequence => Some(sequence.sum / sequence.length)
  }

  // if the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each x in the sequence
  def variance(xs: Seq[Double]): Option[Double] = {
    def mapXsWithMean(m: Double): Seq[Double] = xs.map(x => math.pow(x - m, 2))

    mean(xs).flatMap(m => mean(mapXsWithMean(m)))
  }
}

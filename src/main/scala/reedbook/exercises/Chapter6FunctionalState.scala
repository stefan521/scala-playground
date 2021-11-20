package reedbook.exercises

import RNG._

trait RNG {
  def nextInt: (Int, RNG)
}

// Linear congruential generator
case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE6DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  // Combinators start
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (result1, rng1) = ra(rng)
      val (result2, rng2) = rb(rng1)
      (f(result1, result2), rng2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))
  // Combinators ended

  def int: Rand[Int] = _.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (n, nextRng) if n == Int.MinValue => (Int.MaxValue, nextRng)
      case (n, nextRng) if n < 0 => (-n, nextRng)
      case positiveResult => positiveResult
    }

  def double(rng: RNG): (Double, RNG) =
    rng.nextInt match { case (int, nextRng) => (int.toDouble / Int.MaxValue, nextRng) }

  def doubleWithMap(rng: RNG): (Double, RNG) = map(_.nextInt)(_.toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng1) = rng.nextInt
    val (dbl, rng2) = double(rng1)
    ((int, dbl), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    intDouble(rng) match { case ((int, dbl), nextRng) => ((dbl, int), nextRng) }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (dbl1, rng1) = double(rng)
    val (dbl2, rng2) = double(rng1)
    val (dbl3, rng3) = double(rng2)
    ((dbl1, dbl2, dbl3), rng3)
  }

  def randomInts(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count > 0) rng.nextInt match {
      case (int, nextRng) =>
        val (lst, resultRng) = randomInts(count - 1)(nextRng)
        (int :: lst, resultRng)
    }
    else (List.empty, rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
    case ::(head, tail) =>
      val (headRes, rng1) = head(rng)
      val (tailRes, rng2) = sequence(tail)(rng1)
      (headRes :: tailRes, rng2)

    case Nil => (List.empty, rng)
  }

  def randomIntsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // got to 6.8
}

object Chapter6FunctionalState extends App {
  val seedRNG = SimpleRNG(100L)

  println(randomInts(6)(seedRNG))
}

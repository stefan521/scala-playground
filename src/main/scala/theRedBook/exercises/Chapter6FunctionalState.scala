package theRedBook.exercises

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  val int: Rand[Int] = _.nextInt

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (randomA, rng1) = ra(rng)
    val (randomB, rng2) = rb(rng1)

    (f(randomA, randomB), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    @scala.annotation.tailrec
    def go(lst: List[Rand[A]], nextRng: RNG, lstA: List[A]): (List[A], RNG) = lst match {
      case l if l.isEmpty =>
        (lstA.reverse, nextRng)

      case x::xs =>
        val (nextValue, newRng) = x(nextRng)
        go(xs, newRng, nextValue::lstA)
    }

    go(fs, rng, List.empty)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, nextDouble)

  val randDoubleInt: Rand[(Double, Int)] =
    both(nextDouble, int)

  // this function is of type Rand[Int]
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInt, nextRng) = rng.nextInt
    val nonNegativeRandomInt = math.abs(guardAgainstMinimumInt(randomInt))

    (nonNegativeRandomInt, nextRng)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def nextDouble(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (randomInt, nextRNG) => (randomInt / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def nextDoubleWithMap(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (randomInt, rng1) = rng.nextInt
    val (randomDouble, rng2) = nextDouble(rng1)

    ((randomInt, randomDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((randInt, randDouble), nextRng) = intDouble(rng)

    ((randDouble, randInt), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (first, rng1) = nextDouble(rng)
    val (second, rng2) = nextDouble(rng1)
    val (third, rng3) = nextDouble(rng2)

    ((first, second, third), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(listOfInts: List[Int], nextRng: RNG): (List[Int], RNG) = {
      if (count <= listOfInts.size)
        (listOfInts, nextRng)
      else {
        val (nextRandomInt, newRng) = nextRng.nextInt
        go(nextRandomInt :: listOfInts, newRng)
      }
    }

    go(List.empty, rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (randomValue, nextRng) = f(rng)
    g(randomValue)(nextRng)
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2withFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
  }

  private def guardAgainstMinimumInt(num: Int): Int = num match {
    case Int.MinValue => 0
    case notMinValue => notMinValue
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE6DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

object Chapter6FunctionalState extends App {
  val (myDouble, nextRng) = RNG.nextDouble(SimpleRNG(22))
  val niceFormatDouble = String.format("%.12f", myDouble)

  val (ints, rng) = RNG.ints(10)(SimpleRNG(100))

  println(niceFormatDouble)
  println(ints)
}

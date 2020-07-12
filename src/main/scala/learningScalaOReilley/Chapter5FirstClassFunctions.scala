package learningScalaOReilley

object Chapter5FirstClassFunctions {
  def maxInt: (Int, Int) => Int = (lhs: Int, rhs: Int) => if(lhs > rhs) lhs else rhs

  def compareInts(ints: (Int, Int, Int), comparisonFn: (Int, Int) => Int): Int = {
    val fromFirstTwo = comparisonFn(ints._1, ints._2)
    comparisonFn(fromFirstTwo, ints._3)
  }

  def applyToRandoms(f: (Int, Int) => Int): Int = {
    f(util.Random.nextInt, util.Random.nextInt)
  }

  def getMaxes(givenInts: (Int, Int)): (Int, Int) = {
    val randomMax = applyToRandoms(maxInt)
    val maxGiven = maxInt(givenInts._1, givenInts._2)

    (randomMax, maxGiven)
  }

  def multiplyWith(num: Int): Int => Int = {
    (mult: Int) => num * mult
  }

  def conditional[A](x: A, pred: A => Boolean, func: A => A): A = {
    if(pred(x)) func(x) else x
  }
}

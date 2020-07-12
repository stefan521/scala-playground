package learningScalaOReilley

object Chapter4Functions {
  def computeCircleArea(radius: Double): Double = {
    scala.math.Pi * radius * radius
  }

  def parseComputeCircleArea(radius: String, computeArea: Double => Double): Option[Double] = radius match {
    case null => None
    case _ => try {
      Some(computeArea(radius.toDouble))
    } catch {
      case _: NumberFormatException => None
    }
  }

  @annotation.tailrec
  def printByStep(startVal: Int, stopVal: Int, step: Int): Unit = {
    if(startVal <= stopVal) {
      print(s"$startVal, ")
      printByStep(startVal + step, stopVal, step)
    }
  }

  @annotation.tailrec
  def power(num: Int, toPower: Int, accumulator: Int = 1): Int = {
    if (toPower < 1) accumulator
    else power(num, toPower - 1, accumulator * num)
  }

  def getManhattanDistance(p1: (Double, Double), p2: (Double, Double)): Double = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

  def stringifyTuple[A, B, C](tp: (A, B, C)): (A, String, B, String, C, String) = {
    (tp._1, tp._1.toString, tp._2, tp._2.toString, tp._3, tp._3.toString)
  }
}

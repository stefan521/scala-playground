// Object literals page 45

object Oswald {
  val colour = "black"
  val food = "milk"
}

object Henderson {
  val colour = "ginger"
  val food = "chips"
}

object Quentin {
  val colour = "tabby and white"
  val food = "curry"
}

// better cats
case class Cat(name: String, colour: String, food: String)
val oswald = Cat("Oswald", "black", "milk")
val henderson = Cat("Henderson", "ginger", "chips")
val quentin = Cat("Quentin", "tabby and white", "curry")

object calc {
  def square(num: Double): Double = num * num
  def cube(num: Double): Double = square(num) * num

  def pow(num: Double, power: Int): Double = _pow(num, power)
  def pow(num: Int, power: Int): Double = _pow(num, power)

  @scala.annotation.tailrec
  private def _pow(
    num: Double,
    power: Int,
    accumulator: Double = 1
  ): Double =
    if (power > 0) _pow(num , power - 1, num * accumulator)
    else if (power < 0) _pow(num, power + 1, accumulator / num)
    else accumulator
}

calc.pow(-10, -3)
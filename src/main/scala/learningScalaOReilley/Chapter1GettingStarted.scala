package learningScalaOReilley

object Chapter1GettingStarted {
  def convertCelsiusToFahrenheit(degrees: Double): Double =  degrees * 9 / 5 + 32

  def convertFahrenheitToCelsius(degrees: Double): Double = (degrees - 32) / 9 * 5
}

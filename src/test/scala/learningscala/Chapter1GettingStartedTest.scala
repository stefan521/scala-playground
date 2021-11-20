package learningscala
import Chapter1GettingStarted._
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{FlatSpec, Matchers}

class Chapter1GettingStartedTest extends FlatSpec with Matchers {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.001)

  it should "convert Celsius to Fahrenheit" in {
    assert(convertCelsiusToFahrenheit(22.5) == 72.5)
    assert(convertCelsiusToFahrenheit(0) == 32.0)
    assert(convertCelsiusToFahrenheit(100) == 212.0)
    assert(convertCelsiusToFahrenheit(-10) == 14)
    assert(convertCelsiusToFahrenheit(34) == 93.2)
  }

  it should "converts Fahrenheit to Celsius" in {
    assert(convertFahrenheitToCelsius(72.5) == 22.5)
    assert(convertFahrenheitToCelsius(32) == 0)
    assert(convertFahrenheitToCelsius(212) == 100.0)
    assert(convertFahrenheitToCelsius(14) == -10)
    assert(convertFahrenheitToCelsius(93.2) == 34)
  }
}

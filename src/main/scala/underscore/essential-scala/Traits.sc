import java.nio.file.WatchService
import scala.annotation.tailrec
// page 90

trait Feline {
  def colour: String
  def sound: String
}

trait BigCat extends Feline {
  val sound = "roar"
}

class Cat(val colour: String, val favouriteFood: String) extends Feline {
  val sound = "meow"
}

class Lion(val colour: String, val maneSize: Int) extends BigCat

class Panther(val colour: String) extends BigCat

class Tiger(val colour: String) extends BigCat

case class RGB(red: Double, green: Double, blue: Double)

trait Color {
  def rgb: RGB
}

object  Color {
  def apply(rgbVal: RGB): Color = new Color {
    override val rgb = rgbVal
  }
}

val red = Color(RGB(100, 0, 0))

val yellow = Color(RGB(100, 100, 0))

val pink = Color(RGB(253, 185, 200))

sealed trait Shape {
  def sides: Double
  def perimeter: Double
  def area: Double
}

sealed trait RectangularShape extends Shape {
  def height: Double

  def width: Double

  override val sides = 4

  override val perimeter = height * 2 + width * 2

  override val area = height * width
}

final case class Circle(radius: Double, rgb: RGB) extends Shape with Color {
  override val sides = 0

  override val perimeter = 2 * radius * math.Pi

  override val area = math.Pi * math.pow(radius, 2)
}

final case class Square(sideLength: Double, rgb: RGB) extends RectangularShape with Color {
  override val height: Double = sideLength
  override val width: Double = sideLength
}

final case class Rectangle(height: Double, width: Double, rgb: RGB) extends RectangularShape with Color

object Draw {
  def apply(shape: Shape): String = shape match {

    case circle: Circle =>
      s"A circle of radius ${circle.radius}cm and color ${circle.rgb}"

    case square: Square =>
      s"A square of side length ${square.sideLength}cm and color ${square.rgb}"

    case rectangle: Rectangle =>
      s"A rectangle of width ${rectangle.width}cm and height ${rectangle.height}cm and color ${rectangle.rgb}"
  }
}

Draw(Circle(109, RGB(100, 0, 0)))

sealed trait DivisionResult
case class Finite(result: Double) extends DivisionResult
case object PositiveInfinity extends DivisionResult
case object NegativeInfinity extends DivisionResult


object divide {
  def apply(op1: Double, op2: Double): DivisionResult =
    op2 match {
      case 0.0 if op1 >= 0 => PositiveInfinity
      case 0.0 if op1 <  0 => NegativeInfinity
      case _               => Finite(op1 / op2)
    }
}

divide(11, 2.0)
divide(100, 50)
divide(10, 3)
divide(1, 0)
divide(-2, 0)
divide(0, 0)


sealed trait TrafficLight {
  def next(): TrafficLight = this match {
    case Green  => Yellow
    case Yellow => Red
    case Red    => Green
  }
}
final case object Green extends TrafficLight
final case object Yellow extends TrafficLight
final case object Red extends TrafficLight

Green.next()


sealed trait WaterSource
final case class Well(name: String) extends WaterSource
final case class Spring(name: String) extends WaterSource
final case object Tap extends WaterSource

case class BottledWater(size: Int, source: WaterSource, carbonated: Boolean)


// either
type CalculationEither = Either[String, Int]
// or
sealed trait Calculation
final case class Success(result: Int) extends Calculation
final case class Failure(error: String) extends Calculation

object Calculator {

  def +(calculation: Calculation, num: Int): Calculation =
    calculation match {
      case Success(result)       => Success(result + num)
      case failure @ Failure(_)  => failure
    }

  def -(calculation: Calculation, num: Int): Calculation =
    calculation match {
      case Success(result)       => Success(result - num)
      case failure @ Failure(_)  => failure
    }

  def /(calculation: Calculation, num: Int): Calculation =
    calculation match {

      case Success(result)      =>
        if (num == 0) Failure("Division by zero")
        else Success(result / num)

      case failure @ Failure(_) =>
        failure
    }
}

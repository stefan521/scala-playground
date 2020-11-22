import com.github.nscala_time.time.Imports._

DateTime.now
DateTime.now + 2.months
DateTime.nextMonth < DateTime.now + 2.months
(2.hours + 45.minutes + 10.seconds).millis

/*
 Parsing a Number from a String. All the toSomething methods are from StringLike
 They can still throw NumberFormatException
 */
"100".toInt
"100".toDouble
"100".toFloat

val bigInt = BigInt("1")
val bigDecimal = BigDecimal("3.14159")

val a: Long = 100L
a.isValidByte // from RichDouble
a.isValidInt

Double.PositiveInfinity
Double.NegativeInfinity

val range1 = 1 to 10
val range2 = 1 to 10 by 2

val formatter = java.text.NumberFormat.getIntegerInstance
formatter.format(1000000)
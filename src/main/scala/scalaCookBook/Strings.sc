import scala.util.matching.Regex

val s = "Hello" + " world"

"hello".foreach(println)

for (c <- "hello") println(c)

s.getBytes().foreach(println)

val filteredString = "hello world".filter(_ != 'l')

"scala"
  .drop(2)
  .take(2)
  .capitalize

// s is actually a function defined on StringContext
s"One plus one is ${1 + 1}"

// string interpolation with formatting, like printf
val name = "Pete"
val age = 22
val weight = 88.123
f"$name is $age years old, and weighs $weight%.2f pounds."

raw"look at me \n \\ no escaping, no new lines here"

// from scala 2.11 you can use string interpolation with pattern matching
// before scala 2.10 you don't have the s method so you need to call format
"%s is %d years old".format(name, age)

// Regex
val numPattern = "[0-9]+".r
val address = "123 Main Street Suite 101"
val match1 = numPattern.findFirstIn(address)
address.matches("[0-9]+") // false
val matches = numPattern.findAllIn(address)

val numPatternAgain = new Regex("[0-9]+")
val result = numPattern.findFirstIn(address).getOrElse("no match")
val addressReplace = "123 Main Street".replaceAll("[0-9]", "x")

val pattern = "([0-9]+) ([A-Za-z]+)".r
val pattern(count, fruit) = "100 Bananas"

val MoviesZipRe = "movies (\\d{5})".r
val MoviesNearCityStateRE = "movies near ([a-z]+), ([a-z]{2})".r
"movies 12354" match {
  case MoviesZipRe(zip) => println(s"zip $zip}") // the input string MUST match the whole regex
  case MoviesNearCityStateRE(city, state) => println(s"city $city state $state")
  case _ => println("did not match a regex")
}
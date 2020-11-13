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
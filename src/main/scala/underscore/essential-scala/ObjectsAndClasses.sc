// page 65
trait Food
case object Milk extends Food
case object Chips extends Food
case object Curry extends Food

class Cat(val name: String, val colour: String, val food: Food)

val oswald = new Cat("Oswald", "Black", Milk)
val henderson = new Cat( "Henderson", "Ginger", Chips)
val quentin = new Cat("Quentin", "Tabby and White", Curry)

object ChipShop {
  def willServe(cat: Cat): Boolean = cat.food == Chips
}

class Director(
  val firstName: String,
  val lastName: String,
  val yearOfBirth: Int
) {
  def name() = firstName + " " + lastName

  override def toString: String = s"Director($firstName, $lastName, $yearOfBirth)"
}

class Film(
  val name: String,
  val yearOfRelease: Int,
  val imdbRating: Double,
  val director: Director
) {
  def directorsAge(): Int = yearOfRelease - director.yearOfBirth

  def isDirectedBy(director: Director): Boolean = this.director == director

  def copy(
    name: String = this.name,
    yearOfRelease: Int = this.yearOfRelease,
    imdbRating: Double = this.imdbRating,
    director: Director = this.director
  ) = new Film(name, yearOfRelease, imdbRating, director)

  override def toString: String = s"Film($name, $yearOfRelease, $imdbRating, $director)"
}

val eastwood = new Director("Clint", "Eastwood", 1930)
val mcTiernan = new Director("John", "McTiernan", 1951)
val nolan = new Director("Christopher", "Nolan", 1970)
val someBody = new Director("Just", "Some Body", 1990)
val memento = new Film("Memento", 2000, 8.5, nolan)
val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
val inception = new Film("Inception", 2010, 8.8, nolan)
val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7,
  eastwood)
val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9,
  eastwood)
val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
val invictus = new Film("Invictus", 2009, 7.4, eastwood)
val predator = new Film("Predator", 1987, 7.9, mcTiernan)
val dieHard = new Film("Die Hard", 1988, 8.3, mcTiernan)
val huntForRedOctober = new Film("The Hunt for Red October", 1990,
  7.6, mcTiernan)
val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8,
  mcTiernan)


eastwood.yearOfBirth
dieHard.director.name()
invictus.isDirectedBy(nolan)

highPlainsDrifter.copy(name = "L'homme des hautes plaines")

class Adder(amount: Int) {
  def add(in: Int) = in + amount
}

class Counter(val count: Int = 1) {
  def increment(): Counter = new Counter(count + 1)
  def decrement(): Counter = new Counter(count - 1)
  def adjust(adder: Adder): Counter = new Counter(adder.add(count))
}

new Counter(0)
  .adjust(new Adder(23))
  .increment()
  .adjust(new Adder(6))
  .count

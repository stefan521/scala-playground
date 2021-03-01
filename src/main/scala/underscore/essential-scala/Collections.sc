import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}

val animals = Seq("cat", "dog", "penguin")

"tyrannosaurus" +: animals :+ "mouse"

animals :+ 1 // Seq[Any]

case class Film(
  name: String,
  yearOfRelease: Int,
  imdbRating: Double
)

case class Director(
  firstName: String,
  lastName: String,
  yearOfBirth: Int,
  films: Seq[Film]
)

val memento = new Film("Memento", 2000, 8.5)
val darkKnight = new Film("Dark Knight", 2008, 9.0)
val inception = new Film("Inception", 2010, 8.8)
val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
val unforgiven = new Film("Unforgiven", 1992, 8.3)
val granTorino = new Film("Gran Torino", 2008, 8.2)
val invictus = new Film("Invictus", 2009, 7.4)
val predator = new Film("Predator", 1987, 7.9)
val dieHard = new Film("Die Hard", 1988, 8.3)
val huntForRedOctober = new Film("The Hunt for Red October", 1990,
  7.6)
val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

val eastwood = new Director(
  "Clint",
  "Eastwood",
  1930,
  Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus)
)

val mcTiernan = new Director("John", "McTiernan", 1951,
  Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

val nolan = new Director("Christopher", "Nolan", 1970, Seq(memento, darkKnight, inception))

val someGuy = new Director("Just", "Some Guy", 1990, Seq())

val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

// TODO: Write your code here!

def directedMoreThanAndBornBefore(
  numberOfFilms: Option[Int],
  year: Option[Int]
): Seq[Director] =
  directors.filter(director => {
    val directedFilms = numberOfFilms.forall(requiredFilms => director.films.length > requiredFilms)
    val bornBefore = year.forall(requiredYear => director.yearOfBirth < requiredYear)

    directedFilms && bornBefore
  })

def directedMoreThan(numberOfFilms: Int): Seq[Director] =
  directedMoreThanAndBornBefore(Some(numberOfFilms), None)

def bornBefore(year: Int): Seq[Director] =
  directedMoreThanAndBornBefore(None, Some(year))

nolan.films.toList

directors.flatMap(_.films.toList)

mcTiernan.films.foldLeft(None.asInstanceOf[Option[Int]]) { (earliest, nextFilm) =>
    earliest.map(year => math.min(year, nextFilm.yearOfRelease))
}

directors
  .flatMap(_.films.toList)
  .sortWith { (film1, film2) => film1.imdbRating >= film2.imdbRating }

val films = directors.flatMap(_.films)
films.foldLeft(0.0)((sum, film) => sum + film.imdbRating) / films.
  length

def minimum(sequence: Seq[Int]): Option[Int] = {

  @tailrec
  def findMinimum(seq: Seq[Int], min: Option[Int]): Option[Int] =
    seq match {
      case s if s.isEmpty =>
        min

      case s =>
        val newMin = Some(math.min(s.head, min.getOrElse(s.head)))
        findMinimum(s.tail, newMin)
    }

  findMinimum(sequence, None)
}

def smallest(seq: Seq[Int]): Int =
  seq.foldLeft(Int.MaxValue)(math.min)

val numbers = Seq(18, 2, -9, 123, 12, 0)

minimum(numbers)
smallest(numbers)

val notUnique = Seq(1, 1, 2, 4, 3, 4)

notUnique.toSet

notUnique.foldLeft(Seq.empty[Int]) { (seq, el) =>
  if (seq contains el) seq
  else el +: seq
}

numbers.reverse

def reverse[A](seq: Seq[A]): Seq[A] =
  seq.foldLeft(Seq.empty[A]) { (reversed, el) => el +: reversed }

reverse(numbers)

def map[A, B](seq: Seq[A])(fn: A => B): Seq[B] =
  seq.foldRight(Seq.empty[B]) { (el, mapped) => mapped :+ fn(el) }

map(numbers)(_ + 10)


for {
  film <- nolan.films
} yield film.name

for {
  director <- directors
  film <- director.films
} yield film.name

(
  for {
    director <- directors
    films <- director.films
  } yield films
).sortWith(_.imdbRating > _.imdbRating)

for {
  director <- directors
  film <- director.films
} println(s"Tonight only! ${film.name} by ${director.lastName}!")

def addOptions1(left: Option[Int], right: Option[Int]): Option[Int] =
  for {
    leftValue <- left
    rightValue <- right
  } yield leftValue + rightValue

def addOptions2(left: Option[Int], right: Option[Int]): Option[Int] =
  left.flatMap { leftValue =>
    right.map { rightValue =>
      leftValue + rightValue
    }
  }

addOptions1(Some(2), Some(9))
addOptions2(Some(2), Some(9))
addOptions1(None, Some(12))
addOptions2(None, Some(12))
addOptions1(Some(12), None)
addOptions2(Some(12), None)

// 6.8.3.1
val people = Set(
  "Alice",
  "Bob",
  "Charlie",
  "Derek",
  "Edith",
  "Fred"
)

val ages = Map(
  "Alice" -> 20,
  "Bob" -> 30,
  "Charlie" -> 50,
  "Derek" -> 40,
  "Edith" -> 10,
  "Fred" -> 60
)

val favoriteColors = Map(
  "Bob" -> "green",
  "Derek" -> "magenta",
  "Fred" -> "yellow"
)

val favoriteLolcats = Map(
  "Alice" -> "Long Cat",
  "Charlie" -> "Ceiling Cat",
  "Edith" -> "Cloud Cat"
)

def favoriteColor(person: String): String =
  favoriteColors.getOrElse(person, "beige")

def printColors(): List[String] =
  favoriteColors.values.toList

def accessMap[A, B](someMap: Map[A, B], a: A): Option[B] =
  someMap.get(a)

def favouriteColorOfOldestPerson(): Option[String] = {
  val oldest = ages.foldLeft(
      Option.empty[(String, Int)]
    )((oldestPerson, currentPerson) =>
        for {
          old <- oldestPerson
          oldest = if (old._2 > currentPerson._2) old else currentPerson
        } yield oldest
    )

  oldest.flatMap(person => favoriteColors.get(person._1))
}

def setUnion[A](set1: Set[A], set2: Set[A]): Set[A] =
  set1.foldLeft(set2)((unionSet, el) => unionSet + el)

def mapUnion[A, B](
    map1: Map[A, B],
    map2: Map[A, B]
  )(
    combine: (B, B) => B
  ): Map[A, B] =
  map1.foldLeft(map2)((unionMap, pair) =>
    unionMap.get(pair._1) match {
      case Some(value) =>
        unionMap + (pair._1 -> combine(pair._2, value))
      case None =>
        unionMap
    })


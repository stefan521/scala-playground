// 1.3 Exercise: Printable Library
trait Printable[A] {
  def format(a: A): String
}

object Printable {

  def format[A](a: A)(implicit printable: Printable[A]): String =
    printable.format(a)
}

object PrintableInstances {

  implicit val printableString: Printable[String] =
    str => str

  implicit val printableInt: Printable[Int] =
    int => int.toString
}

object PrintableSyntax {

  implicit class PrintableOps[A](a: A) {

    def format(implicit printable: Printable[A]): String =
      printable.format(a)

    def print(implicit  printable: Printable[A]): Unit =
      System.out.println(printable.format(a))
  }
}

import PrintableSyntax._
import cats.{Show, Eq}
import cats.implicits._

final case class Cat(name: String, age: Int, color: String)

val cat = Cat("Roland", 2, "brown")

implicit val printableCat: Printable[Cat] =
  cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."

cat.print

// 1.4.6 Exercise: Cat Show
implicit val showCat: Show[Cat] =
  cat =>  s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."

cat.show

// 1.5.5 Exercise: Equality, Liberty, and Felinity
implicit val eqCat: Eq[Cat] =
  (lhs, rhs) => lhs.color === rhs.color && lhs.name == rhs.name && lhs.age == rhs.age

val cat1 = Cat("Garfield", 38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")
val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

cat1 === cat2
optionCat1 === optionCat2
cat1 =!= cat2
optionCat1 =!= optionCat2
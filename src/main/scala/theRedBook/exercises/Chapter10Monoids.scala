package theRedBook.exercises

class Chapter10Monoids extends App {
  trait Monoid[A] {
    def  op(a: A, b: A): A // must be associative (parens don't matter)
    def zero: A // op(zero, a) must always give a same for op(a, zero)
  }

  val stringMonoid = new Monoid[String] {
    override def op(a: String, b: String): String = a + b // concatenation

    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a: List[A], b: List[A]): List[A] = a ++ b

    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a || b

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a && b

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)

    override def zero: Option[A] = None
  }

  // a monoid for endofunctions (the domain is the same as the codomain
  def endoMonoid[A]: Monoid[A => A] =  new Monoid[A => A]{
    override def op(a: A => A, b: A => A): A => A =
      (finallyAnArgument: A) => b(a(finallyAnArgument))

    override def zero: A => A = identity[A]
  }
}

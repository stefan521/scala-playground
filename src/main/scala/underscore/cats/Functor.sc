import cats.Functor
import cats.implicits._

// 3.5.4 Exercise: Branching out with Functors
sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {

  override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value) => Leaf(f(value))
  }
}

// Variance problem at compile time. Can use smart constructors.
val tree1: Tree[Int] = Leaf(2)
val tree2: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

tree1.map(_ * 5)
tree2.map(_ + 10)

// 3.6.1.1 Exercise: Showing off with Contramap
trait Printable[A] {

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    (value: B) => format(func(value))
}

def format[A](value: A)(implicit p: Printable[A]): String =
  p.format(value)

// 3.6.2.1 Transformative Thinking with imap
trait Codec[A] { self =>

  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B): String =
      self.encode(enc(value))

    override def decode(value: String): B =
      dec(self.decode(value))
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String =
  c.encode(value)

def decode[A](value: String)(implicit c: Codec[A]): A =
  c.decode(value)

implicit val stringCodec: Codec[String] =
  new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }


final case class Box[A](value: A)

implicit val codecDouble: Codec[Double] =
  stringCodec.imap(_.toDouble, _.toString)

implicit def codecBox[A](implicit c: Codec[A]): Codec[Box[A]] =
  c.imap[Box[A]](
    (v: A) => Box(v),
    (v: Box[A]) => v.value
  )

encode(123.4)
encode(Box(123.4))


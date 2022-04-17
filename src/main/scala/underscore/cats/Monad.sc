import cats.data.Reader
import cats.{Eval, MonadError}
import cats.implicits._

import scala.util.Try

// 4.1.2 Exercise: Getting Func‐y
trait Monad[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value) { a: A => pure(func(a)) }
}

// 4.3.1 Exercise: Monadic Secret Identities
type Id[A] = A

def pureId[A](a: A): Id[A] = a

def flatMapId[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

def mapId[A, B](value: Id[A])(func: A => B): Id[B] = func(value)

// 4.5.4 Exercise: Abstracting
def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
  if(age >= 18)
    age.pure[F]
  else
    new IllegalArgumentException(
      "Age must be greater than or equal to 18"
    ).raiseError[F, Int]


validateAdult[Try](18)
validateAdult[Try](8)
type ExceptionOr[A] = Either[Throwable, A]
validateAdult[ExceptionOr](-1)

/**
 * EVAL monad - evaluation strategies, stack-safe
 * Now - eager, not memorized (like val)
 * Always - lazy, not memorized (like def)
 * Later - lazy, memorized (like lazy val)
 *
 * */
val now = Eval.now(math.random + 1000)
val always = Eval.always(math.random + 3000)
val later = Eval.later(math.random + 2000)

now.value
now.value

always.value
always.value

later.value
later.value

val saying = Eval
  .always{ println("Step 1"); "The cat" }
  .map{ str => println("Step 2"); s"$str sat on" }
  .memoize // Memorize chain so far
  .map{ str => println("Step 3"); s"$str the mat" }

saying.value

// 4.6.5 Exercise: Safer Folding using Eval
def foldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
  as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRight(tail, acc)(fn)))
    case Nil =>
      acc
  }

// 4.8.3 Exercise: Hacking on Readers
final case class Db(
  usernames: Map[Int, String],
  passwords: Map[String, String]
)

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader { Db => Db.usernames.get(userId) }

def checkPassword(username: String, password: String): DbReader[Boolean] =
  Reader { Db => Db.passwords.get(username).contains(password) }

def checkLogin(userId: Int, password: String): DbReader[Boolean] =
  findUsername(userId).flatMap {
    case Some(userName) => checkPassword(userName, password)
    case None => false.pure[DbReader]
  }

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)

val passwords = Map(
  "dade" -> "zerocool",
  "kate" -> "acidburn",
  "margo" -> "secret"
)

val db = Db(users, passwords)

checkLogin(1, "zerocool").run(db)

checkLogin(4, "davinci").run(db)

// 4.9.3 Exercise: Post‐Order Calculator
import cats.data.State

type CalcState[A] = State[List[Int], A]

def operand(num: Int): CalcState[Int] =
  State { stack => (num :: stack, num) }

def operator(func: (Int, Int) => Int): CalcState[Int] =
  State {
    case b :: a :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)

    case _ =>
      throw new IllegalStateException("Stack is messed up")
  }

def evalOne(sym: String): CalcState[Int] =
  sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case number => operand(number.toInt)
  }

evalOne("42").runA(Nil).value

def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState]) {
    case (state, symbol) => state.flatMap(_ => evalOne(symbol))
  }

val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
multistageProgram.runA(Nil).value





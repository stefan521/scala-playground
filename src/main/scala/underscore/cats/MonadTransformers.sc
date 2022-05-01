import cats.instances.list._
import cats.syntax.applicative._
import cats.data.OptionT

import scala.concurrent.Future

/*
 *
 *  Monad transformers help us avoid nested flatmap calls when working with multiple monad types.
 *
 * Knowing the type of one monad allows us to generalize the definition of map and flatmap to compose it
 * with another monad and avoid nested flatmap calls.
 *
 * A monad transformer is not just a type alias. It is a data type (class) that makes code nicer.
 *
 */
type ListOption[A] = OptionT[List, A]

val result1: OptionT[List, Int] = OptionT(List(Option(10), Option(20), None))

val result2: OptionT[List, Int] = 32.pure[ListOption]

result1.flatMap { (x: Int) =>
  result2.map { (y: Int) =>
    x + y
  }
}

package reedbook.exercises

import java.util.concurrent.{ExecutorService, TimeUnit, Future => Jfuture, Callable}

private case class UnitFuture[A](get: A) extends Jfuture[A] {

  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  override def isCancelled: Boolean = false

  override def isDone: Boolean = true

  override def get(timeout: Long, unit: TimeUnit): A = get
}

object Par {

  type Par[A] = ExecutorService => Jfuture[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def map2[A, B, C](parA: Par[A], parB: Par[B])(fn: (A, B) => C): Par[C] =
    es => new Jfuture[C] {
      private val futureA = parA(es)
      private val futureB = parB(es)
      @volatile private var cache: Option[C] = None

      override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
        val cancelledA = futureA.cancel(mayInterruptIfRunning)
        val cancelledB = futureB.cancel(mayInterruptIfRunning)
        cancelledA && cancelledB
      }

      override def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled

      override def isDone: Boolean = futureA.isDone && futureB.isDone

      override def get(): C = fn(futureA.get, futureB.get)

      override def get(timeout: Long, unit: TimeUnit): C =
        cache.getOrElse {
          val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, unit)
          val started = System.nanoTime
          val a = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
          val elapsed = System.nanoTime - started
          val b = futureB.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)
          val c = fn(a, b)
          cache = Some(c)
          c
        }
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](fn: A => B): A => Par[B] = arg => lazyUnit(fn(arg))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    es => unit(
      ps.foldLeft(List.empty[A]) { case (acc, parEl) => parEl(es).get :: acc }
    )(es)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def run[A](s: ExecutorService)(a: Par[A]): Jfuture[A] = a(s)
}

object Chapter7Parallelism extends App {

}

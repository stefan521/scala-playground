//package monix
//
//import monix.catnap.{CircuitBreaker, MVar}
//import monix.eval.Task
//import monix.execution.ExecutionModel
//import monix.execution.Scheduler.{global => scheduler}
//import monix.execution.atomic.Atomic
//import monix.execution.Scheduler.Implicits.global
//
//import scala.concurrent.duration.DurationInt
//
//// https://monix.io/docs/3x/
//object MonixPlayground extends App {
//
//  // ********** monix-execution
//
//  /*
//    A scheduler enables asynchronous execution of code. It extends an ExecutionContext.
//    It can execute Runnables.
//   */
//  scheduler.execute(new Runnable {
//    override def run(): Unit = println("execute with scheduler")
//  })
//
//  // We can schedule things with a delay. If we change our mind later we can cancel them.
//  // program will terminate even if stuff is still scheduled.
//  val cancellable = scheduler.scheduleOnce(5.seconds) {
//    println("Hello, world!")
//  }
//
//  // schedule at intervals
//  scheduler.scheduleWithFixedDelay(3.seconds, 5.seconds) {
//    println("Fixed delay task")
//  }
//
//  //ExecutionModel
//  ExecutionModel.BatchedExecution //  DEFAULT executes batches asynchronously. within one batch work is executed synchronously
//  ExecutionModel.SynchronousExecution // execute things synchronously
//  ExecutionModel.AlwaysAsyncExecution // execute everything asynchronously
//
//  // monix offers an Atomic interface that can box references as well as primitives
//  val ref = Atomic(BigInt(1))
//
//  ref.addAndGet(BigInt(1231231))
//
//  // ********** monix-catnap
//
//  /*
//  A Circuit Breaker is an FSM that helps you avoid resource exhaustion/ unnecessary work. It's like a semaphore...
//  It can be Open, Half-Open or Closed.
//  Open = Fail all requests
//  Closed = Allow all requests
//  Half-open = Test if we can close the breaker
//  It counts exceptions. Too many and the circuit breaker opens.
//   */
//
//  val circuitBreaker: Task[CircuitBreaker[Task]] =
//    CircuitBreaker[Task].of(
//      maxFailures = 5,
//      resetTimeout = 10.seconds
//    )
//
//  val problematic = Task {
//    val nr = util.Random.nextInt()
//    if (nr % 2 == 0) nr else
//      throw new RuntimeException("dummy")
//  }
//
//  for {
//    ci <- circuitBreaker
//    r  <- ci.protect(problematic)
//  } yield r
//
//  /*
//    MVar
//    - an atomic mutable variable, also available in cats effects.
//    - the take and put operations are atomic.
//   */
//  def sum(state: MVar[Task, Int], list: List[Int]): Task[Int] =
//    list match {
//      case Nil => state.take
//      case x :: xs =>
//        state.take.flatMap { current =>
//          state.put(current + x).flatMap(_ => sum(state, xs))
//        }
//    }
//
//  // monix-eval
//
//  val task =
//    for {
//      state <- MVar[Task].of(0)
//      r <- sum(state, (0 until 100).toList)
//    } yield r
//
//  task.runToFuture.foreach(println)
//
//  var effect = 0
//
//  val source = Task.eval {
//    effect += 1
//    if (effect < 3) throw new RuntimeException("dummy") else effect
//  }
//
//  val cached = source.memoizeOnSuccess
//  val cachedAnyway = source.memoize
//
//  val ta = Task { println("Effect1"); 1 }
//  val tb = Task { println("Effect2"); 2 }
//
//  // takes a sequence of tasks and returns a task of sequence
//  val list: Task[Seq[Int]] = Task.sequence(Seq(ta, tb))
//
//  // there's also Task.parSequence (sequence in parallel). Get better performance from Task.parSequenceUnordered
//
//  Task.never // makes a Task that never completes
//
//  // Executing things in parallel
//  val locationTask: Task[String] = Task.now("UK")
//  val phoneTask: Task[String] = Task.now("07123")
//  val addressTask: Task[String] = Task.now("address")
//  import cats.syntax.all._
//
//  (locationTask, phoneTask, addressTask).parMapN {
//    (location, phone, address) => "Gotcha!"
//  }
//
//  // there is also Coeval. It's like Task but it can only run things synchronously. It's good for controlling side effects.
//  // Coeval is like cats Eval but it offers a better API for error handling.
//
//  // monix-reactive
//}

package cats.effects

import cats.effect.{ExitCode, IO, IOApp}


object Concepts extends IOApp {

  /*
  IO is like a monix Task. It is a description of a computation. It runs on fibres. A fibre is a lightweight thread that
  is completely managed by the runtime (Cats Effects). It is much cheaper than OS threads. You can spawn tens of millions
  of fibres.

  Easy tracing built in. Easy test integrations.

  Resource management is taken care of so even if something times out or errors it releases resources.
  */
  val helloWorld: IO[Unit] =
    for {
      _ <- IO.println("hello 1")
      _ <- IO.println("world 1")
    } yield ()

  // the >> operator ignores the result of the previous effect. It has the same meaning as *>
  val helloWorldSyntax: IO[Unit] = IO.println("hello 2") >> IO.println("world 2")

  override def run(args: List[String]): IO[ExitCode] = helloWorldSyntax.as(ExitCode.Success)
}

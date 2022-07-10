package cats.effects

import cats.effect.{ExitCode, IO, IOApp}

object Baeldung extends IOApp {
  def putStr(str: String): IO[Unit] = IO.delay(println(str))

  val launch = for {
    _ <- putStr("Lauch missiles")
    _ <- putStr("Lauch missiles")
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = launch.as(ExitCode.Success)
}

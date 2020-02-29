import org.scalatest.FunSpec

object TestCase extends FunSpec {
  def assertTestCases[A, B](testCases: List[TestCase[A, B]], f: A => B): Unit = {
      testCases.foreach(testCase => assert(f(testCase.input) == testCase.output))
  }
}

case class TestCase[A, B](input: A, output: B)

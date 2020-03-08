package learningScala

import Chapter3ExpressionsAndConditionals._
import org.scalatest.FunSpec

class Chapter3ExpressionsAndConditionalsTest extends FunSpec {
  describe("checks names exist") {
    it("approves names") {
      assert(checkName("Peter") == "Peter")
      assert(checkName("Maria") == "Maria")
      assert(checkName("Gibberish") == "Gibberish")
    }

    it("rejects empty and null names") {
      assert(checkName(null) == "n/a")
      assert(checkName("") == "n/a")
    }
  }

  describe("compares with zero") {
    it("returns 'greater' for positive numbers") {
      assert(compareWithZero(111.512) == "greater")
      assert(compareWithZero(0.21) == "greater")
      assert(compareWithZero(55) == "greater")
    }

    it("returns 'less' for negative numbers") {
      assert(compareWithZero(-0.0002) == "less")
      assert(compareWithZero(-1030.921) == "less")
      assert(compareWithZero(-218421) == "less")
    }

    it("returns 'same' for 0") {
      assert(compareWithZero(0) == "same")
      assert(compareWithZero(0.0) == "same")
    }
  }

  it("checks colors") {
    assert(convertColor("cyan") == "#00FFFF")
    assert(convertColor("magenta") == "#FF00FF")
    assert(convertColor("yellow") == "#FFFF00")
    assertThrows[IllegalArgumentException](convertColor("notAColor"))
  }
}

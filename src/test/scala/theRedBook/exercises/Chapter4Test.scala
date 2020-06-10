package theRedBook.exercises

import org.scalatest.{FunSpec, Matchers}
import Chapter4._

class Chapter4Test extends FunSpec with Matchers {
  describe("Option") {
    describe("when None") {
      it("map should return None") {
        MyNone map (_ => 22) shouldBe MyNone
      }

      it("flatMap should return None") {
        MyNone flatMap (_ => MySome(123)) shouldBe MyNone
      }

      it("getOrElse should return the default value") {
        MyNone.getOrElse(MyNone) shouldBe MyNone
        MyNone.getOrElse(912) shouldBe 912
        MyNone.getOrElse(MySome(92)) shouldBe MySome(92)
      }

      it("orElse should return the default Option") {
        MyNone.orElse(MySome(941)) shouldBe MySome(941)
        MyNone.orElse(MyNone) shouldBe MyNone
        MyNone.orElse(MySome("hello")) shouldBe MySome("hello")
      }

      it("filter should return None") {
        MyNone.filter(_ => true) shouldBe MyNone
        MyNone.filter(_ => false) shouldBe MyNone
      }
    }

    describe("when Some") {
      it("map should map the value of Some") {
        MySome(123) map (_ * 2) shouldBe MySome(246)
        MySome(123) map (_.toString) shouldBe MySome("123")
      }

      it("flatMap should handle failure when applying the anonymous function") {
        MySome(123) flatMap(_ => MyNone) shouldBe MyNone
        MySome(123) flatMap(num => MySome(num * 2)) shouldBe MySome(246)
      }

      it("getOrElse should flatten the Some") {
        MySome(521).getOrElse(92) shouldBe 521
      }

      it("orElse should return the Some") {
        MySome(521).orElse(MySome(1)) shouldBe MySome(521)
        MySome(521).orElse(MyNone) shouldBe MySome(521)
      }

      it("filter should keep the Some(s) that pass the filter") {
        MySome(10) filter (n => n < 6) shouldBe MyNone
        MySome(10) filter (n => n > 6) shouldBe MySome(10)
      }
    }
  }

  describe("mean") {
    it("should return None for an empty sequence") {
      mean(Seq.empty) shouldBe None
    }

    it("should return the single element in a sequence of length one") {
      mean(Seq(521)) shouldBe Some(521)
    }

    it("should return the average of the sequence members") {
      mean(Seq(2.0, 2.0, 2.0)) shouldBe Some(2.0)
      mean(Seq(1, 2, 3)) shouldBe Some(2.0)
      mean(Seq(5, 15, 10)) shouldBe Some(10.0)
      mean(Seq(2, 5)) shouldBe Some(3.5)
      mean(Seq(0, 9, 0, 1)) shouldBe Some(2.5)
    }
  }
}

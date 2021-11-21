package reedbook.exercises

import org.scalatest.{Matchers, WordSpec}
import Chapter03FunctionalDataStructures._

object Chapter3FunctionalDataStructuresTest {
  val leafTree: Tree[Int] = Leaf(5)

  val simpleTree: Tree[Int] = Branch(
    Leaf(1),
    Leaf(21)
  )

  val biggerTree: Tree[Int] = Branch(
    Branch(
      Branch(
        Leaf(21),
        Leaf(321)
      ),
      Branch(
        Leaf(521),
        Leaf(1)
      )
    ),
    Branch(
      Leaf(51),
      Leaf(25)
    )
  )
}
class Chapter3FunctionalDataStructuresTest extends WordSpec with Matchers {
  import Chapter3FunctionalDataStructuresTest._

  "appendListWithFold" should {
    "append two empty lists" in {
      appendListWithFold(Nil, Nil) shouldBe Nil
    }

    "if one list is empty keep the elements form the other list" in {
      appendListWithFold(List(1, 5, 6), Nil) shouldBe List(1, 5, 6)
      appendListWithFold(Nil, List(1, 5, 6)) shouldBe List(1, 5, 6)

      appendListWithFold(List("cactus", "palm", "tree"), Nil) shouldBe List("cactus", "palm", "tree")
      appendListWithFold(Nil, List("cactus", "palm", "tree")) shouldBe List("cactus", "palm", "tree")
    }

    "append the elements of the left list at the front of the second list" in {
      appendListWithFold(
        List("cactus", "palm", "tree"),
        List("moss", "grass")
      ) shouldBe List("cactus", "palm", "tree", "moss", "grass")

      appendListWithFold(List(-1, -3, 2, -5), List(2, 8, 9)) shouldBe List(-1, -3, 2, -5, 2, 8, 9)
    }
  }

  "addListElements" should {
    "add integers on the same index from different list" in {
      addListElementsTogether(List(1, 2, 3), List(5, 9, 11)) shouldBe List(6, 11, 14)
      addListElementsTogether(List(1), List(5, 4, 2)) shouldBe List(6, 4, 2)
      addListElementsTogether(List(1, 5, 12), List(9)) shouldBe List(10, 5, 12)
      addListElementsTogether(List(1, 5, 12), Nil) shouldBe List(1, 5, 12)
      addListElementsTogether(Nil, List(1, 5, 12)) shouldBe List(1, 5, 12)
    }
  }

  "zipListsWith" should {
    "zip two lists" in {
      zipListsWith(List(1, 2, 3), List("tree", "leaf", "branch"))((a, b) => s"$a-$b") shouldBe List(
        "1-tree",
        "2-leaf",
        "3-branch"
      )

      zipListsWith(List(1, 2, 3), List[String]())((a, b) => s"$a-$b") shouldBe List(
        1,
        2,
        3
      )

      zipListsWith(List[Int](), List("tree", "leaf", "branch"))((a, b) => s"$a-$b") shouldBe List(
        "tree",
        "leaf",
        "branch"
      )

      zipListsWith(List(1, 2), List("tree", "leaf", "branch"))((a, b) => s"$a-$b") shouldBe List(
        "1-tree",
        "2-leaf",
        "branch"
      )
    }
  }

  "treeSize" should {
    "return the number of nodes in a tree" in {
      treeSize(leafTree) shouldBe 1

      treeSize(simpleTree) shouldBe 3

      treeSize(biggerTree) shouldBe 11
    }
  }

  "maximumElement" should {
    "return the node with the maximum value in a tree of Int" in {
      maximumElement(leafTree) shouldBe 5

      maximumElement(simpleTree) shouldBe 21

      maximumElement(biggerTree) shouldBe 521
    }
  }

  "depth" should {
    "return the maximum depth of a tree" in {
      depth[Int](leafTree) shouldBe 1

      depth[Int](simpleTree) shouldBe 2

      depth[Int](biggerTree) shouldBe 4
    }
  }

  "map" should {
    "transform the leaves of a tree" in {
      mapTree(leafTree)(num => num + 105) shouldBe Leaf(110)

      mapTree(simpleTree)(a => a.toString + "-leaf") shouldBe Branch(Leaf("1-leaf"), Leaf("21-leaf"))
    }
  }
}

import PrettyPrint._
import org.scalatest._

class PrettyPrintSuite extends FunSuite {
  test("test1") {
    val expected = """+-Root
                     |  +-level1-1
                     |  +-level1-2
                     |  +-level1-3""".stripMargin
    val result = asciiDisplay(TreeNode("Root",
      children = List(TreeNode("level1-1"),
        TreeNode("level1-2"),
        TreeNode("level1-3")))).mkString("\n")

    assert( result === expected)
  }

  test("test2") {
    val expected ="""+-Root
        |  +-level1-1
        |  | +-level2-1
        |  |   +-level3-1
        |  |
        |  +-level1-2
        |  +-level1-3""".stripMargin

    val result = asciiDisplay(TreeNode("Root",
      children = List(
        TreeNode("level1-1", children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
        TreeNode("level1-2"),
        TreeNode("level1-3")))).mkString("\n")

    assert (expected === result)
  }

  test("test3") {
    val expected = """+-Root
                     |  +-level1-1
                     |  | +-level2-1
                     |  | | +-level3-1
                     |  | | +-level3-2
                     |  | |
                     |  | +-level2-2
                     |  |
                     |  +-level1-2
                     |  | +-level2-3
                     |  |
                     |  +-level1-3""".stripMargin

    val result = asciiDisplay(TreeNode("Root",
      children =
        List(
          TreeNode("level1-1",
            children = List(TreeNode("level2-1",
              children = List(TreeNode("level3-1"), TreeNode("level3-2"))),
              TreeNode("level2-2"))),
          TreeNode("level1-2",
            children = List(TreeNode("level2-3"))
          ),
          TreeNode("level1-3")
        ))).mkString("\n")

    assert(expected === result)
  }
}

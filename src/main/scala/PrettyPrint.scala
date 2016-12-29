case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

object PrettyPrint {

  def pprint(prefix: String, hasNext: Boolean, root: TreeNode[String]): Seq[String] = {
    val currentLine = prefix + "+-" + root.data.toString

    val nextPrefix = prefix + (if (hasNext) "| " else "  ")

    val children = root.children
    val n = children.length

    if (children.isEmpty) {
      Seq(currentLine)
    } else {
      currentLine +: children
        .zipWithIndex
        .map { case (child, idx) =>
          pprint(nextPrefix, idx != n - 1, child)
        }
        .reduceRight{ (x, acc) =>
          if (x.length == 1) {
            x ++ acc
          } else {
            x ++ Seq(nextPrefix + "|") ++ acc
          }
        }
    }
  }

  def asciiDisplay(root: TreeNode[String]): Seq[String] = {
    pprint("", false, root)
  }

  def main(args: Array[String]): Unit = {

  }
}

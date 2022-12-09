package adventofcode.y2022

case class Tree[A](elem: A, children: Seq[Tree[A]]):
  def fold[B](combine: (A, Seq[B]) => B): B =
    val ch = this.children.map(_.fold(combine))
    combine(this.elem, ch)
  def map[B](f: A => B): Tree[B] = fold[Tree[B]]((a, chb) => Tree(f(a), chb))

object Tree:
  def unfold[A](start: A)(next: A => Seq[A]): Tree[A] = Tree(start, next(start).map(unfold(_)(next)))

@main
def Day07 = withResource("day07a.txt") {

  // tuples of commands, each with their 'console output', if any
  val commandsOutputs: Seq[(String, Seq[String])] = Seq.unfold(lines()) {
    case s"$$ ${head}" +: tail =>
      val (ls, rest) = tail.span(!_.startsWith("$"))
      Some((head, ls), rest)
    case Seq() => None
  }

  enum Node:
    case Dir
    case File(size: Long)

  case class Path(path: Seq[String], node: Node)

  val elemsAtPaths: Seq[Path] =
    commandsOutputs.foldLeft((Seq[String](), Seq[Path]())) { case ((cwd, acc), (cmd, out)) =>
      cmd match {
        case "cd /" => (Seq(), acc)
        case "cd .." => (cwd.init, acc)
        case s"cd $dir" => (cwd :+ dir, acc)
        case "ls" =>
          (cwd, acc ++ out.map {
            case s"dir ${d}" => Path(cwd :+ d, Node.Dir)
            case s"${AsInt(size)} ${file}" => Path(cwd :+ file, Node.File(size))
          })
      }
    }._2

  val tree: Tree[Path] =
    Tree.unfold[Path](Path(Seq[String](), Node.Dir)) {
      case Path(currentPath, _) => elemsAtPaths.collect { case path@Path(init :+ _, _) if init == currentPath => path }
    }

  val treeSized: Tree[(Path, Long)] = tree.fold {
    case (path@Path(_, Node.Dir), children) => Tree((path, children.map(_.elem._2).sum), children)
    case (path@Path(_, file: Node.File), _ ) => Tree((path, file.size), Seq())
  }

  val dirSizes: Seq[Long] = treeSized.fold {
    case ((Path(_, Node.Dir), size), children) => children.flatten :+ size
    case (_, children) => Seq()
  }

  // part 1
  println(dirSizes.filter(_ <= 100000).sum)

  val sizeRoot = treeSized.elem._2
  val available = 70000000L - sizeRoot
  val needed = 30000000 - available

  // part 2
  println(dirSizes.filter(_ >= needed).min)
}

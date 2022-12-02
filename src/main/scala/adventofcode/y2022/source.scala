package adventofcode.y2022

// helpers to reduce file reading boilerplate (which already hardly existed anyway...)

opaque type Source = () => scala.io.Source

def withResource[A](fileName: String)(thunk: Source ?=> A): A = {
  val mkSrc = () => Option(getClass.getResourceAsStream(s"/adventofcode/y2022/${fileName}")).map { is =>
    scala.io.Source.fromInputStream(is)
  }.getOrElse(sys.error("Resource not found"))
  thunk(using mkSrc)
}

def lines()(using src: Source): Seq[String] = src().getLines().toSeq

def intsFirstLine(): Source ?=> Seq[Int] = lines().head.split(',').toSeq.map(_.toInt)
def intLines(): Source ?=> Seq[Int] = lines().filterNot(_.isBlank).map(_.toInt)

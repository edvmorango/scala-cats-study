package implicits

object CalculableInstance {

  implicit val stringWriter: CalculableWriter[String] = {
    new CalculableWriter[String] {
      override def write(value: String): Calculable = CalculableString(value)
    }
  }

}

object CalculableInterface {

  def toCalculable[A](value: A)(implicit w: CalculableWriter[A]): Calculable =
    w.write(value)

}

object CalculableSyntax {

  implicit class CalculableWriterOps[A](value: A) {
    def toCalculable(implicit w: CalculableWriter[A]): Calculable =
      w.write(value)
  }

}

object MainImplicit extends App {
  import CalculableInstance._

  val interface = {
    CalculableInterface.toCalculable("123123")
  }
  val syntax = {
    import CalculableSyntax._

    "123123".toCalculable
  }

  println(s"Interface: ${interface.getClass} ")
  println(s"Syntax: ${syntax.getClass}")

}

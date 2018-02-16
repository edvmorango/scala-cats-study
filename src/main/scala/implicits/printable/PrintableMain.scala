package implicits.printable

import implicits.calculable.{Calculable, CalculableWriter}

object PrintableInstances {

  implicit val printableStringFormatter: PrintableFormatter[String] = {
    new PrintableFormatter[String] {
      override def format(value: String): Printable = PrintableValue(value)

      override def print(value: String): Unit = println(s"Print -> $value")
    }
  }

  implicit val printableIntFormatter: PrintableFormatter[Int] = {
    new PrintableFormatter[Int] {
      override def format(value: Int): Printable =
        PrintableValue(s"Int Printable form: ${value.toString}")
      override def print(value: Int): Unit = println(s"Print -> $value")

    }
  }

}

object PrintableInterface {

  def format[A](v: A)(implicit f: PrintableFormatter[A]) = f.format(v)

  def print[A](v: A)(implicit p: PrintableFormatter[A]) = p.print(v)

}

object PrintableSyntax {

  implicit class PrintableFormatterOps[A](value: A) {
    def asPrintable(implicit f: PrintableFormatter[A]): Printable =
      f.format(value)

    def print(implicit p: PrintableFormatter[A]): Unit =
      p.print(value)

  }

}

object PrintableMain extends App {
  import PrintableInstances._

  val interfaces = {
    val printable = PrintableInterface.format(10)
    println(printable)
    PrintableInterface.print(20)
  }

  val syntax = {
    import PrintableSyntax._
    val printable = 50.asPrintable
    println(printable)
    60.print
  }

}

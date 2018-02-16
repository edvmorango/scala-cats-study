package implicits.printable

final case class Person(name: String, age: Int, status: String)

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

  implicit val printablePersonFormatter: PrintableFormatter[Person] = {
    new PrintableFormatter[Person] {
      override def format(v: Person): Printable =
        PrintableValue(s"${v.name} have ${v.age} years and is ${v.status}")

      override def print(v: Person): Unit =
        println(s"Print Person: ${v.toString}")
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

  val person = new Person("Eduardo", 21, "Healthy")

  val interfaces = {
    val p = PrintableInterface.format(person)
    println(p)
    PrintableInterface.print(person)
  }

  val syntax = {
    import PrintableSyntax._
    val p = person.asPrintable
    println(p)
    person.print
  }

}

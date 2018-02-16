package implicits.printable

sealed trait Printable

final case class PrintableValue(value: String) extends Printable

trait PrintableFormatter[A] {

  def format(value: A): Printable

  def print(value: A): Unit

}

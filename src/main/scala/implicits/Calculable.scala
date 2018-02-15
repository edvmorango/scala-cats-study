package implicits

//Type class come from Haskell so this concept don't work without ADTs(Algebric)

sealed trait Calculable

final case class CalculableString(value: String) extends Calculable

trait CalculableWriter[A] {

  def write(value: A): Calculable

}

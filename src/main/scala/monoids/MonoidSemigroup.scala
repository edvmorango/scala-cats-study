package monoids

trait Semigroup[A] {
  def append(a: A, b: A): A
}

trait MonoidS[A] extends Semigroup[A] {
  def id: A
}

package monoids

trait Monoid[A] {
  def id(): A
  def append(a: A, b: A): A
}

object MonoidProofVerifier {

  def associativityLaw[A](a: A, b: A, c: A)(
      implicit monoid: Monoid[A]): Boolean = {

    monoid.append(monoid.append(a, b), c) == monoid.append(a,
                                                           monoid.append(b, c))
  }

}

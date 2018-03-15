package applicative
import cats.{Semigroupal => CSemigroupal}
import cats.instances.option._

object SemigroupalOptionInstance {
  implicit val semigroupalInstance = new Semigroupal[Option] {

    def product[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = {
      (a, b) match {
        case (_, None)          => None
        case (None, _)          => None
        case (Some(a), Some(b)) => Some(a, b)
      }

    }

  }

}

object SemigroupalMain extends App {

  // Product of (A x None) = None
  println(CSemigroupal[Option].product(Some(1), Some("two")))
  println(CSemigroupal[Option].product(None, Some("")))

  //Tuples methods seems invariants
  println(CSemigroupal.tuple3(Option(1), Option("hey"), Option('c')))

  println(CSemigroupal.map3(Option(1), Option(2), Option(3))(_ * _ * _))

}

object ApplicativeMain extends App {
  import cats.Monoid
  import cats.syntax.apply._
  import cats.instances.int._
  import cats.instances.list._
  import cats.instances.string._
  import cats.syntax.semigroup._
  import cats.instances.invariant._

  case class Variadic(a: Int, b: String, c: Double, d: Char)

  // Tupled is variadic
  println((Option(1), Option("hey")).tupled)

  // Apply arguments into a wrapped structure/function A   --> F[A]   same Haskell <*>
  val variadicApply =
    (Option(1), Option("two"), Option(3.0), Option('4')).mapN(Variadic.apply)

  println(s"Variadic Apply: $variadicApply")

  val functionApply = (Option(2), Option(2)).mapN((a, b) => a * b)

  println(s"Function Apply: $functionApply")

  //Monoid
  case class Monoidic(name: String, priority: Int, dependencies: List[Int])

  val tupleToMonoidic: (String, Int, List[Int]) => (Monoidic) =
    Monoidic.apply _

  val monoidicToTuple: Monoidic => (String, Int, List[Int]) = m =>
    (m.name, m.priority, m.dependencies)

  implicit val monoidicMonoid: Monoid[Monoidic] =
    (Monoid[String], Monoid[Int], Monoid[List[Int]])
      .imapN(tupleToMonoidic)(monoidicToTuple)

  val empty = Monoidic("", 0, Nil)
  val monoid1 = Monoidic("A", 1, List(1, 2, 3))
  val monoid2 = Monoidic("B", 1, List(4, 5, 6))

  println(s"Monoidic: ${empty |+| monoid1 |+| monoid2}")
}

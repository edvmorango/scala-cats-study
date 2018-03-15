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
  import cats.syntax.apply._

  case class Variadic(a: Int, b: String, c: Double, d: Char)

  // Tupled is variadic
  println((Option(1), Option("hey")).tupled)

  // Apply arguments into a wrapped structure/function A   --> F[A]   same Haskell <*>
  val variadicApply =
    (Option(1), Option("two"), Option(3.0), Option('4')).mapN(Variadic.apply)

  println(s"Variadic Apply: $variadicApply")

  val functionApply = (Option(2), Option(2)).mapN((a, b) => a * b)

  println(s"Function Apply: $functionApply")
}

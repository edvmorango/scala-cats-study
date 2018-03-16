package applicative
import cats.{Monad, Semigroupal => CSemigroupal}
import cats.instances.option._

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}
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

object SemigroupalDifferentTypes extends App {
  // Semigroupal is monadic.

  import cats.instances.invariant._
  import cats.Monoid
  import cats.instances.future._
  import cats.instances.list._
  import cats.instances.either._
  import scala.concurrent.Future
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.syntax.apply._
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val f1 = Future {
    println("F1 started")
    Thread.sleep(1000)
    println("F1 finishing")
    "F1 finished"
  }

  val f2 = Future {
    println("F2 started")
    Thread.sleep(2000)
    println("F2 finishing")
    2000
  }

  val fs = CSemigroupal[Future].product(f1, f2)

  val fr = Await.ready(fs, Duration.Inf).value

  println(fr)

  //List

  val ls = CSemigroupal[List].product(List(1, 2, 3), List("a", "b", "c"))

  println(ls)

  //Either

  type EitherA[A] = Either[List[String], A]

  val els = CSemigroupal[EitherA]
    .product(Left(List("First fail")), Left(List("Second fail")))
  val ers = CSemigroupal[EitherA].product(Right(10), Right(20))
  val erl = CSemigroupal[EitherA].product(Right(10), Left(List("Second fail")))

  println(els)
  println(ers)
  println(erl)

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)

}

object ValidatedMain extends App {

  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._
  import cats.instances.int._
  import cats.syntax.validated._
  import cats.syntax.apply._
  import cats.syntax.semigroup._
  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.data.Validated.Invalid

  type EitherAcc[A] = Validated[List[String], A]

  val validated =
    CSemigroupal[EitherAcc]
      .product(Invalid(List("First fail")), Invalid(List("Second fail")))

  println(validated)

  val monoid = 123.valid[List[String]] |+| List("Second")
    .invalid[Int] |+| List("Third").invalid[Int]

  println(s"Monoid $monoid")

  val pre = 123.pure[EitherAcc]
  val raised = List("Raised error").raiseError[EitherAcc, Int]

  println(pre)
  println(raised)

  println
  // Helpers

  val fromExc = Validated.catchOnly[NumberFormatException]("string".toInt)
  val fromTry = Validated.fromTry(Try { 10 / 0 })
  val fromEitherValid = Validated.fromEither(Right("Some valid string"))

  println(s"Exc: ${fromExc}")
  println(s"Try: ${fromTry}")
  println(s"Valid Either: ${fromEitherValid}")

}

object ExerciseMain extends App {

  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._
  import cats.data.Validated.{Invalid, Valid}
  import cats.syntax.semigroup._

  import cats.syntax.apply._

  case class User(name: String, age: Int)

  type UserValidation[A] = Validated[List[String], A]

  val validDb: Map[String, String] = Map("name" -> "Eduardo", "age" -> "22")
  val invalidDb: Map[String, String] = Map("nam" -> "Eduardo", "age" -> "-1")

  def readName(db: Map[String, String]): UserValidation[String] = {
    db.get("name") match {
      case None => Invalid(List("Empty name"))
      case Some(name) =>
        if (name.isEmpty) Invalid(List("Blank Name")) else Valid[String](name)
    }
  }

  def readAge(db: Map[String, String]): UserValidation[Int] = {
    db.get("age") match {
      case None => Invalid(List("Empty age"))
      case Some(age) =>
        Try(age.toInt) match {
          case Success(age) =>
            if (age > 0) Valid[Int](age) else Invalid(List("Invalid age"))
          case _ => Invalid(List("Invalid age format"))
        }
    }
  }

  def readUser(db: Map[String, String]): UserValidation[User] = {
    (readName(db), readAge(db)).mapN(User.apply)

  }

  println(readName(validDb))
  println(readName(invalidDb))

  println(readAge(validDb))
  println(readAge(invalidDb))

  println(readUser(validDb))
}

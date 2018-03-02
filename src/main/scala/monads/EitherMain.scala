package monads

import cats.syntax.either._



object EitherMain extends App{

  val ea : Either[String, Int] = 10.asRight[String].ensure("Must be greather than 9")(x => x > 9)
  val eb = "Fail".asLeft[Int]

  println(ea)
  println(eb)

}

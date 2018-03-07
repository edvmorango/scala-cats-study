package monads

import cats.data.Reader


object ReaderMain extends App{

  case class Integer(v: Int)

  type IntegerStringR = Reader[Integer, String]


  val catReader: IntegerStringR = Reader(  integer =>  s"My injected dependency is: ${integer.v}")

  println(catReader.run(Integer(10)))


  val double: IntegerStringR = Reader( int => s"Double of injected ($int) is: ${int.v * 2}")
  val cube: IntegerStringR = Reader( int => s"Cube of injected ($int) is: ${int.v ^ 3}")


  val calc = for {
     d <- double
     c <- cube
  } yield  s"$d\n$c"


  val res = calc.run(Integer(3))
  val uRes: String = res // Unwrap from Id monad
  println("\nComposition: ")
  println(uRes)


}

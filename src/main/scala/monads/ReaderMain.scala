package monads

import cats.data.{Reader, ReaderWriterState}
import cats.syntax.applicative._

object ReaderMain extends App{

  case class Integer(v: Int)

  type IntegerStringR = Reader[Integer, String]


  val catReader: IntegerStringR = Reader(  integer =>  s"My injected dependency is: ${integer.v}")

  println(catReader.run(Integer(10)))


  val double: IntegerStringR = Reader( int => s"Double of injected ($int) is: ${int.v * 2}")
  val cube: IntegerStringR = Reader( int => s"Cube of injected ($int) is: ${int.v * int.v * int.v}")


  val calc = for {
     d <- double
     c <- cube
  } yield  s"$d\n$c"

  val res = calc.run(Integer(3))
  val uRes: String = res // Unwrap from Id monad
  println("\nComposition:")
  println(uRes)



  val desugared = double.flatMap{ d =>
    cube.map(c =>  s"$d\n$c")
  }

  val desRes = desugared.run(Integer(3))
  println("\nDesugared composition:")
  println(desRes)


}

object ReaderExercise extends App {

  case class Db(users: Map[Int, String], passwords: Map[String, String])

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo")

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret")


  val dbInstance = Db(users, passwords)

  type DbReader[A] = Reader[Db, A]

  def findUser(id: Int): DbReader[Option[String]] =
    Reader( db => db.users.filter(_._1 == id).values.headOption)

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader( db => db.passwords.exists( u =>  u._1 == username && u._2 == password ))

  def checkLogin(userId: Int , password: String): DbReader[Boolean] = for {
   usrOpt <- findUser(userId)
   login <- usrOpt.map(usr => checkPassword(usr ,password)).getOrElse( false.pure[DbReader] )
  } yield login


 println(checkLogin(1, "zerocool").run(dbInstance))
 println(checkLogin(4, "davinci").run(dbInstance))


}
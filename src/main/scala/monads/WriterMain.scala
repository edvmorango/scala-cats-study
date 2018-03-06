package monads

import cats.data.{Writer, WriterT}
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.int._
import cats.instances.vector._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object WriterMain extends App {


  //  val s1 = "Step 1".pure

  val test = Writer(1, "Step 1")

  val v  = test.value
  val w  = test.written
  val r = test.run

  println(v)
  println(w)
  println(r)

  println("\n")

  type LogCount[A] = Writer[Int, A]

  val mc1 = for {
    a <- "Step 1".pure[LogCount]
    _ <- 1.tell
    b <-  ";Step 2".writer(2)
  } yield a + b

  println(mc1)

  println("\n")

  type LogString[A] = Writer[Vector[String], A]

  val mc2 = for {
    a <- 1.pure[LogString]
    _ <- Vector("\nStep 1 Happening1").tell
    _ <- Vector("\nStep 1 still happening").tell
    b <- 2.writer(Vector("\nStep 2 starting"))
    _ <- Vector("\nStep 2 still happening").tell
  } yield  {
    val sum =  a + b

    // Tell Will not work, out of monad context
    Vector(s"\n$a + $b resulted: $sum").tell
    Vector(s"\n FINISHED!").tell

    // not happening mappend, for this log.
    sum.writer(Vector("\nThis time finished\n"))
  }
  println(mc2)



  // Monadic future.

  println("\n")

  lazy val f =  Future { 1.pure[LogString] }
  lazy val f2 = Future { 2.writer(Vector("Step 2 happening")) }

  val mt: Future[LogString[Int]] =  for {
    aw <- f
    bw <- f2
  } yield {
    for {
      a <- aw
      _ <- Vector("Step 1 happening").tell
      b <- bw
    } yield a + b
  }
  val fin = Await.ready(mt, Duration.Inf).value.get

  println(s"Fin: $fin")





}

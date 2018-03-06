package monads

import cats.data.{Writer, WriterT}
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.int._
import cats.instances.vector._

import scala.concurrent.duration.{Duration, DurationLong}
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

  println(s"Monadic Future: $fin")

  println("\n")

  // Applicative Future


  //for some reason it seems that writer Monad always maintains sequence
  // I guess that is based on evaluation order.

  val fa = Future {
    println("Evaluating step 1")

    Thread.sleep(5000)
    val v = 1.writer(Vector("Step 1 begun"))

    // Won't happen because are out of Writer Functor
    Vector("Step 1 begun...").tell
    Vector("Step 1 finished...").tell

    println("Step 1 Happened")

    v
  }

   val fa2 = Future {
     println("Evaluating step 2")
     Thread.sleep(1000)

     val v = 2.writer(Vector("Step 2 begun..."))

     println("Step 2 Happened")

     v
  }

  val af: Future[LogString[Int]] =for {
    (aw, bw) <- fa zip fa2
  } yield  {
    for {
      a <- aw
      b <- bw
    } yield b + a // Sum order don't matter on vector order, so writer is associative?
  }

  val finAp = Await.ready(af, Duration.Inf).value.get

  println(s"Applicative Future: $finAp")




}

object WriterExercise extends App {


  type LogString[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): LogString[Int] = {



    for {
      ans <- if (n == 0) {
        n.pure[LogString]
      } else {
      factorial( n - 1).map(_ * n)
      }
      _ <- Vector(s"Thread ${Thread.currentThread().getName} N: ${n} ").tell
    } yield ans

  }


  val x = Await.result(Future.sequence(Vector(
    Future{
      Thread.sleep(500)
      factorial(3).run
    },
    Future(factorial(5).run)
  )), 10.seconds)

  println(x)
  x.map(v => v._1.map(l => println(l)  ) )


}
package mtransformers

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.flatMap._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object MonadTransformerMain extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels =
    Map("Vegeta" -> 8001,
        "Goku" -> 8000,
        "Trunks" -> 6000,
        "Goten" -> 4000,
        "Kuririn" -> 5,
        "Yamcha" -> -1)

  //Pure infers Future from cats.instance
  def getPowerLevel(zfighter: String): Response[Int] =
    powerLevels.get(zfighter) match {
      case None    => EitherT.left("Not found".pure)
      case Some(a) => EitherT.right(a.pure)
    }

  def fusionCanAchieveSSJ3(zfighter: String,
                           zfigher2: String): Response[Boolean] =
    for {
      f <- getPowerLevel(zfighter)
      s <- getPowerLevel(zfigher2)
    } yield (f + s) >= 10000

  def fusion(zfighter: String, zfighter2: String): Response[String] =
    for {
      f <- fusionCanAchieveSSJ3(zfighter, zfighter2)
    } yield {
      if (f)
        s"$zfighter and $zfighter2 are dancing"
      else
        s"The earth is doomed"
    }

  val can =
    Await.result(fusionCanAchieveSSJ3("Trunks", "Yamcha").value, Duration.Inf)
  val fuse =
    Await.result(fusion("Vegeta", "Goku").value, Duration.Inf)

  println(s"Vegeta power ${getPowerLevel("Vegeta")}")
  println(s"Trunks + Yamcha fusion can achieve SSJ3? ${can}")
  println(s"Goku + Vegeta vs Boo: $fuse")

}

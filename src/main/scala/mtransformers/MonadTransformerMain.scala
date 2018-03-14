package mtransformers

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MonadTransformerMain extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels =
    Map("Vegeta" -> 8001, "Goku" -> 8000, "Kuririn" -> 5, "Yamcha" -> -1)

  //Pure infers Future from cats.instance
  def getPowerLevel(zfighter: String): Response[Int] =
    powerLevels.get(zfighter) match {
      case None    => EitherT.left("Not found".pure)
      case Some(a) => EitherT.right(a.pure)
    }

  println(s"Vegeta power ${getPowerLevel("Vegeta")}")

}

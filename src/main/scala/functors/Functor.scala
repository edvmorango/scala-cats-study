package functors
import scala.language.higherKinds
import cats.instances.option._

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

}

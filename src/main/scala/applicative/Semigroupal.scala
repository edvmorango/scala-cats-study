package applicative

trait Semigroupal[F[_]] {

  def product[A, B](a: F[A], b: F[B]): F[(A, B)]
}

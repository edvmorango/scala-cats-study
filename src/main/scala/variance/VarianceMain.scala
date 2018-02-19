package variance

trait Covariant[+A] {
  def message(): A // Cannot require A as parameter
}

trait Contravariant[-A] {
  def message(a: A): Unit // Cannot return A
}

class Fruit()
class Grape() extends Fruit

object VarianceMain extends App {

  val grape = new Grape
  val fruit = new Fruit

  //In covariance, the focus are into specialized types
  val cov: Covariant[Fruit] = new Covariant[Grape] {
    override def message(): Grape = {
      println("This call is covariant, accepts a child impl")
      new Grape
    }
  }

  // Gets the father and acts like him.
  val contra: Contravariant[Grape] = new Contravariant[Fruit] {
    override def message(a: Fruit): Unit = {
      println(
        "Contravariant requires a grape as parameter but accepts a Fruit impl")
    }
  }

  cov.message()
  contra.message(new Grape)

}

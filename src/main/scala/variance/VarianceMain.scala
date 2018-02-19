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

  //get a child and return the father
  val cov: Covariant[Fruit] = new Covariant[Grape] {
    override def message(): Fruit = {
      println("This call is covariant, accepts a child impl")
      new Fruit
    }
  }

  // gets any family member and can specialize it
  val cov2: Covariant[Fruit] = new Covariant[Fruit] {
    override def message(): Fruit = {
      println("This call is covariant who receives any fruit, return any child")
      new Grape
    }
  }

  // Gets the father and can specialize him.
  val contra: Contravariant[Grape] = new Contravariant[Fruit] {
    override def message(a: Fruit): Unit = {
      println(
        "Contravariant requires a grape as parameter but accepts a Fruit impl")
    }
  }

  cov.message()
  cov2.message()

  contra.message(new Grape)

}

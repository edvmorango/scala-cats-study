package monoids

case class Container(boxCount: Int)

object ContainerMonoidInstance {

  implicit val containerMonoid = new Monoid[Container] {

    override def id(): Container = Container(0)

    override def append(a: Container, b: Container): Container =
      Container(a.boxCount + b.boxCount)

  }

}

object ContainerMonoidInterface {

  def mappend[A](a: A, b: A)(implicit monoid: Monoid[A]) = monoid.append(a, b)

}

object ContainerMonoidSyntax {

  implicit class MonoidOps[A](a: A) {
    def append(b: A)(implicit m: Monoid[A]): A =
      m.append(a, b)
  }

}

object BasicMonoidMain extends App {
  import ContainerMonoidInstance._

  val container1 = Container(1)
  val container2 = Container(2)
  val container3 = Container(3)

  val biggerContainer = ContainerMonoidInterface.mappend(container1, container2)

  val superContainer =
    ContainerMonoidInterface.mappend(biggerContainer, container3)

  println(s"Bigger Container Box Count: ${biggerContainer.boxCount}")
  println(s"Super Container Box Count: ${superContainer.boxCount}")

  val withSyntax = {
    import ContainerMonoidSyntax._

    val c1 = Container(10)
    val c2 = Container(15)
    val c3 = Container(25)

    c1.append(c2).append(c3)
  }

  println(s"Super with Syntax ${withSyntax.boxCount}")

  println(s"Container respect associativity law: ${MonoidProofVerifier
    .associativityLaw(container1, container2, container3)} ")

}

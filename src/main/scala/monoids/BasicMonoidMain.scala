package monoids

case class Container(boxCount: Int)

object ContainerMonoidInstance {

  implicit val containerMonoid = new Monoid[Container] {

    override def id(): Container = Container(0)

    override def append(a: Container, b: Container): Container =
      Container(a.boxCount + b.boxCount)

  }

}

object MonoidInterface {

  def mappend[A](a: A, b: A)(implicit monoid: Monoid[A]) = monoid.append(a, b)

  def catamorphism[A](l: List[A])(implicit monoid: Monoid[A]): A = {
    l.foldRight(monoid.id)(monoid.append)
  }

}

object MonoidSyntax {

  implicit class MonoidOps[A](a: A) {
    def append(b: A)(implicit m: Monoid[A]): A =
      m.append(a, b)
    def appendList(l: List[A])(implicit m: Monoid[A]): A =
      l.foldRight(a)(m.append)
  }

}

object BasicMonoidMain extends App {
  import ContainerMonoidInstance._

  val container1 = Container(1)
  val container2 = Container(2)
  val container3 = Container(3)

  val biggerContainer = MonoidInterface.mappend(container1, container2)

  val superContainer =
    MonoidInterface.mappend(biggerContainer, container3)

  println(s"Bigger Container Box Count: ${biggerContainer.boxCount}")
  println(s"Super Container Box Count: ${superContainer.boxCount}")

  val withSyntax = {
    import MonoidSyntax._

    val c1 = Container(10)
    val c2 = Container(15)
    val c3 = Container(25)

    c1.append(c2).append(c3)
  }

  val listWithSyntax = {

    import MonoidSyntax._

    val currentContainer = Container(50)
    val l = List(Container(100), Container(20), Container(80))

    currentContainer.appendList(l)
  }

  println(s"Super with Syntax ${withSyntax.boxCount}")

  println(s"Container respect associativity law: ${MonoidProofVerifier
    .associativityLaw(container1, container2, container3)} ")

  val list = List(container1, container2, container3)

  println(s"By catamorphism: ${MonoidInterface.catamorphism(list).boxCount}")
  println(s"By list: ${listWithSyntax.boxCount}")

}

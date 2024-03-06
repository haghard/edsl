package patches

//https://github.com/kitlangton/scala-3-compile-time-business/blob/main/src/main/scala/lab/Zippable.scala

object Zippable extends App {

  type Concat[X <: Tuple, +Y] = X match {
    case EmptyTuple => Y
    case x1 *: xs1  => x1 *: Concat[xs1, Y]
  }

  type Zip[A, B] = (A, B) match {
    case (?, Unit)  => A
    case (Unit, ?)  => B
    case (Tuple, b) => Concat[A, b]
    case (a, Tuple) => a *: B
  }

  def zip[A, B](a: A, b: B): Zip[A, B] = {
    val res = (a.asInstanceOf[Matchable], b.asInstanceOf[Matchable]) match {
      case ((), b)              => b
      case (a, ())              => a
      case (a: Tuple, b: Tuple) => a ++ b
      case (a: Tuple, b)        => a :* b
      case (a, b: Tuple)        => a *: b
      case (a, b)               => (a, b)
    }
    res.asInstanceOf[Zip[A, B]]
  }

  extension [A](a: A) {
    def ><[B](b: B): Zip[A, B] =
      Zippable.zip(a, b)
  }

  case class Box[A](value: A) {
    def zip[B](that: Box[B]): Box[Zip[A, B]] = Box(value >< that.value)

    def map[B](f: A => B): Box[B] = Box(f(value))
  }
  // end Box

  val a1: (Int, String, Double, Boolean) = (1, "cool") >< (3.5, true)
  println(a1)

  val a2: (Int, String, Int, Boolean, Int) = (1, "cool") >< (3, false, 5)
  println(a2)

  val a3: (Int, Int) = () >< (3, 4)
  println(a3)

  val a4: (Int, String) = (1, "cool") >< ()
  println(a4)

  val a5: Unit = () >< ()
  println(a5)

  val boxInt = Box(1)
  val boxStrings = Box(("hello", "world"))
  val zipped = boxInt.zip(boxStrings)
  val mapped = zipped.map {
    case (a, b, c) =>
      // a + ", " + b + c
      s"$a, ${b + c}"
  }

  println(zipped)
  println(mapped)
}

package patches

import compiletime.constValue
import compiletime.ops.int.S
import scala.reflect.ClassTag

//https://github.com/scala/scala3/blob/main/tests/run/Providers.scala
//https://www.youtube.com/watch?v=gLJOagwtQDw
object TupleValueExtractor {

  /** The index of the first element type of the tuple `Xs` that is a subtype of `X` */
  type IndexOf[T <: Tuple, A] <: Int =
    T match {
      case A *: _  => 0
      case _ *: ys => S[IndexOf[ys, A]]
    }

  /** A trait describing a selection from a tuple `Xs` returning an element of type `X` */
  trait Extract[T <: Tuple, A] {
    def apply(xs: T): A
  }

  /** A given implementing `Extract` to return the first element of tuple `T` that has a static type matching `A`.
    */
  given [T <: NonEmptyTuple, A](using index: ValueOf[IndexOf[T, A]], tag: ClassTag[A]): Extract[T, A] with {
    override def apply(xs: T): A = {
      println(s"Extract type:${tag.runtimeClass.getName},ind:${index.value}")
      xs.apply(index.value).asInstanceOf[A]
    }
  }

  type TP = (Int, String, List[Int])

  def checkTuple[T](v: T, x: TP)(using selector: Extract[TupleValueExtractor.TP, T]): Unit = {
    val bool = selector(x) == v
    println(bool)
  }

  @main def testTupleExtractor = {
    val x: TP = (42, "hi", List(1, 2, 3))

    checkTuple(11, x) // true
    checkTuple("hi", x) // true
    checkTuple(List(1, 2), x) // false

    val selectInt = summon[Extract[TP, Int]]
    println(selectInt(x))

    val selectString = summon[Extract[TP, String]]
    println(selectString(x))

    val selectListInts = summon[Extract[TP, List[Int]]]
    println(selectListInts(x))

    // val selectListStrs = summon[Extract[TP, List[String]]]

    val selectObject = summon[Extract[TP, Object]]
    println(selectObject(x))

    val selectAnyRef = summon[Extract[TP, AnyRef]]
    println(selectAnyRef(x))

  }
}

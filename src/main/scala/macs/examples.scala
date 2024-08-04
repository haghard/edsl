package macs

//https://github.com/MateuszKubuszok/Scala2vsScala3MacrosExamples/tree/master
@main def app(): Unit = {

  val a = impl.isCaseClass[Some[Int]]
  println(classOf[Some[Int]].getName + " isCaseClass " + a)

  val b = impl.isCaseClass[Option[String]]
  println(classOf[Option[String]].getName + " isCaseClass " + b)

  println(impl.inc(4))

  type A = Option[String] | Int | Option[Int]

  def check(o: A): Option[String] =
    o match {
      case None            => None
      case i: Int          => Some(i.toString)
      case Some(i: Int)    => Some(i.toString)
      case Some(s: String) => Some(s)
    }

  println(s" ${check(6)} / ${check(Some(6))}")
}

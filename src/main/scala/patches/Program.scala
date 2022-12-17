package patches

//runMain patches.Main("")
object Program:

  // def main(args: Array[String]) =
  @main def Main(args: String*): Unit =
    // val list: List[Ops[Show]] = List(Ops[Show](A("a")), Ops[Show](B("b")))
    // println(list.map(_.use(_.show)))

    import patches.Tips3.*
    val columns: List[Column[Trm]] =
      List(Column[Trm](A("a", 1)), Column[Trm](B("b", 3.14)), Column[Trm](C("c", 'z')))

    println(columns.map(_.use(_.term.getClass.getSimpleName)))

    val boolVal = false
    println(CNF("(0v1)^(2v-3)", true, false, boolVal, false)) // (true || false) && (boolVal || !false)

  end Main

package patches

//patches.Program.Main("")
object Program:

  // def main(args: Array[String]) =
  @main def Main(args: String*): Unit =
    // val list: List[Ops[Show]] = List(Ops[Show](A("a")), Ops[Show](B("b")))
    // println(list.map(_.use(_.show)))

    import patches.Tips3.*
    val columns: List[Column[Trm]] =
      List(Column[Trm](A("a", 1)), Column[Trm](B("b", 3.14)), Column[Trm](C("c", 'z')))

    println(columns.map(_.use(_.term.getClass.getSimpleName)))

  end Main

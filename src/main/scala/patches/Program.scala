// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package patches

@main def expr(args: String*): Unit = {
  // val list: List[Ops[Show]] = List(Ops[Show](A("a")), Ops[Show](B("b")))
  // println(list.map(_.use(_.show)))

  import patches.Tips3.*
  val columns: List[Column[Trm]] =
    List(Column[Trm](A("a", 1)), Column[Trm](B("b", 3.14)), Column[Trm](C("c", 'z')))

  println(columns.map(_.use(_.term.getClass.getSimpleName())))

  val boolVal = false
  println(CNF("(0v1)^(2v-3)", true, false, boolVal, false)) // (true || false) && (boolVal || !false)
}

// end Main

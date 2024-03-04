import sbt._

lazy val `edsl` = (project in file(".")).settings(commonSettings)

lazy val scalac3Settings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-explain",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    //"-Xfatal-warnings",

    //"-Yexplicit-nulls",
    //"-Wunused",
    "-Ykind-projector",
    "-Ysafe-init", //guards against forward access reference
    "-Wunused:all",
    "-no-indent", //forces to use braces
    //"-Xfatal-warnings"
  ) ++ Seq("-rewrite"/*, "-indent"*/) ++ Seq("-source", "future-migration")
)

lazy val commonSettings = scalac3Settings ++ Seq(
  name := "edsl",
  organization := "haghard",
  version := "0.0.1-SNAPSHOT",
  startYear := Some(2021),

  Test / parallelExecution := false,
  run / fork := false,

  Compile / console / scalacOptions --= Seq("-Wunused:_", "-Xfatal-warnings"),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,

  //sbt headerCreate
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),

  //https://scala-lang.org/blog/2023/05/30/scala-3.3.0-released.html

  scalaVersion := "3.4.0",
  //scalaVersion := "3.3.1",
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2021-23 by Vadim Bondarev
       |This software is licensed under the Apache License, Version 2.0.
       |You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.
       |""".stripMargin
  ))
)

scalafmtOnCompile := true

resolvers ++= Seq(Resolver.jcenterRepo, "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/")

val AmmoniteVersion = "3.0.0-M0-32-96e851cb"
libraryDependencies ++= Seq(
  "com.softwaremill.quicklens" %% "quicklens" % "1.8.10",
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

  "dev.zio" %% "zio-constraintless" % "0.3.2",

  //"com.lihaoyi" % "ammonite" % AmmoniteVersion % "test" cross CrossVersion.full
)

//Compile / scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports", "-deprecation")

promptTheme := ScalapenosTheme

scalafmtOnCompile := true

Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value)

Test / sourceGenerators += Def.task {
  val file = (Test / sourceManaged).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue


addCommandAlias("c", "compile")
addCommandAlias("r", "reload")
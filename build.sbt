import sbt._

lazy val `edsl` = (project in file(".")).settings(commonSettings)

lazy val scalac3Settings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    //"-Yexplicit-nulls",
    //"-Wunused",
    "-Ykind-projector",
    "-Ysafe-init", //guards against forward access reference
  ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future")
)

lazy val commonSettings = scalac3Settings ++ Seq(
  name := "edsl",
  organization := "haghard",
  version := "0.0.1-SNAPSHOT",
  startYear := Some(2021),

  Test / parallelExecution := false,
  run / fork := false,

  Compile / console / scalacOptions --= Seq("-Wunused:_", "-Xfatal-warnings"),

  //sbt headerCreate
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalaVersion := "3.1.1",
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2021-22 by Vadim Bondarev
       |This software is licensed under the Apache License, Version 2.0.
       |You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.
       |""".stripMargin
  ))
)

scalafmtOnCompile := true

resolvers ++= Seq(Resolver.jcenterRepo, "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/")

libraryDependencies ++= Seq(
  //https://mvnrepository.com/artifact/com.lihaoyi/ammonite_3.1.1
  //("com.lihaoyi" % "ammonite"  % "2.5.2"  % "test").cross(CrossVersion.full)
  //("com.lihaoyi" %% "ammonite" % "2.5.1-7-cd989427" % "test").cross(CrossVersion.full)
  // ("com.lihaoyi" % "ammonite"  % "2.5.0"  % "test").cross(CrossVersion.full) for scala 3.0.2
)

//Compile / scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports", "-deprecation")

promptTheme := ScalapenosTheme

scalafmtOnCompile := true

Test / sourceGenerators += Def.task {
  val file = (Test / sourceManaged).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue


addCommandAlias("c", "compile")
addCommandAlias("r", "reload")
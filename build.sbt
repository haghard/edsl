import sbt._

lazy val ScalafixOrganizeImportsVersion = "0.6.0"

lazy val `edsl` = project.in(file(".")).settings(commonSettings)

lazy val scalac3Settings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-explain",
    "-feature",
    "-language:implicitConversions",
    "-language:adhocExtensions",
    //"-language:strictEquality",
    "-unchecked",

    //"-Yexplicit-nulls",
    //"-Wunused",

    //"-Wunused:all",
    //"-Wvalue-discard",

    //"-Ykind-projector",
    "-Ysafe-init", //guards against forward access reference
    
    //"-Wconf:msg=Marked as deprecated in proto file:silent",
    "-Wconf:msg=pattern selector should be an instance of Matchable:silent",

    "-Xfatal-warnings",
    "-no-indent", // forces to use braces
  ) ++ Seq("-rewrite"/*, "-indent"*/) ++ Seq("-source", "future-migration")
)

lazy val commonSettings = scalac3Settings ++ Seq(
  name := "edsl",
  organization := "haghard",
  version := "0.0.1-SNAPSHOT",
  startYear := Some(2021),

  Test / parallelExecution := false,
  run / fork := false,

  //Compile / console / scalacOptions --= Seq("-Wunused:_", "-Xfatal-warnings"),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,

  semanticdbEnabled := true,
  // Scalafix
  ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % ScalafixOrganizeImportsVersion,

  //sbt headerCreate
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),

  //https://scala-lang.org/blog/2023/05/30/scala-3.3.3-released.html
  scalaVersion := "3.4.0",
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2021-24 by Vadim Bondarev
       |This software is licensed under the Apache License, Version 2.0.
       |You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.
       |""".stripMargin
  ))
)

scalafmtOnCompile := true

resolvers ++= Seq(Resolver.jcenterRepo, "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/")

libraryDependencies ++= Seq(
  "com.softwaremill.quicklens" %% "quicklens" % "1.9.7",
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

  "dev.zio" %% "zio-constraintless" % "0.3.2",
)

//Compile / scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports", "-deprecation")

promptTheme := ScalapenosTheme

scalafmtOnCompile := true

Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value)

addCommandAlias("c", "scalafmt;compile")
addCommandAlias("r", "reload")
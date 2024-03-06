addSbtPlugin("io.spray"           %   "sbt-revolver"    % "0.9.1")
addSbtPlugin("com.scalapenos"     %   "sbt-prompt"      % "1.0.2")
addSbtPlugin("org.scalameta"      %   "sbt-scalafmt"    % "2.5.1")
addSbtPlugin("de.heikoseeberger"  %   "sbt-header"      % "5.0.0")
addSbtPlugin("com.thesamet"       %   "sbt-protoc"      % "1.0.4")
addSbtPlugin("com.timushev.sbt"   %   "sbt-rewarn"      % "0.1.3")
addSbtPlugin("ch.epfl.scala"      %   "sbt-scalafix"    % "0.11.0")

addSbtPlugin("com.eed3si9n"       %   "sbt-buildinfo"   %  "0.11.0")

//https://scalameta.org/docs/semanticdb/guide.html#consuming-semanticdb
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.7.8" cross CrossVersion.full)

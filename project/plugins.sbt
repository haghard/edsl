addSbtPlugin("io.spray"           %   "sbt-revolver"    % "0.9.1")
addSbtPlugin("com.scalapenos"     %   "sbt-prompt"      % "1.0.2")
addSbtPlugin("org.scalameta"      %   "sbt-scalafmt"    % "2.4.6")
addSbtPlugin("de.heikoseeberger"  %   "sbt-header"      % "5.0.0")
addSbtPlugin("com.thesamet"       %   "sbt-protoc"      % "1.0.4")
addSbtPlugin("com.timushev.sbt"   %   "sbt-rewarn"      % "0.1.3")
addSbtPlugin("ch.epfl.scala"      %   "sbt-scalafix"    % "0.10.1")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.10"
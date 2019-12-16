lazy val root = (project in file(".")).
settings(
    name := "fluffy-octo-spork",
    assemblyJarName in assembly := "fluffy_octo_spork.jar",
    version := "0.1",
    libraryDependencies ++= Seq (
      "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2" )
  )

lazy val root = (project in file(".")).
settings(
    name := "fluffy-octo-spork",
    assemblyJarName in assembly := "fluffy_octo_spork.jar",
    version := "0.1",
    libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10+"
  )

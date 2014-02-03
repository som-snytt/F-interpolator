
scalaVersion := "2.11.0-M8"

scalacOptions ++= Seq(/*"-Ymacro-debug-verbose",*/ "-deprecation", "-feature", "-Xlint", "-Xfatal-warnings", "-Xfuture")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "junit" % "junit" % "4.10"
)

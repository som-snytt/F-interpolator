
scalaVersion := "2.11.0-M7"
//scalaVersion := "2.11.0-M8"
//scalaVersion := "2.11.0-SNAPSHOT"

//scalaHome := Some(file("/home/apm/projects/snytt/build/pack"))
//scalaHome := Some(file("/home/apm/projects/edge/build/pack"))
//scalaHome := Some(file("/home/apm/clones/scala/build/pack"))

//logBuffered in Test := false

resolvers += "Local Maven Repository" at "file:///home/apm/.m2/repository"

scalacOptions ++= Seq(/*"-Xdev",*/ /*"-Ymacro-debug-verbose",*/ "-deprecation", "-feature", "-Xlint", "-Xfatal-warnings")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  //"com.chuusai" %% "shapeless" % "1.2.4",
  //"org.scalaz" %% "scalaz-core" % "7.0.3"
  //"com.github.som-snytt" %% "expecty" % "0.9" % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "junit" % "junit" % "4.10"
)

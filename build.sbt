name := "Leftist Heaps"

organization := "com.qtamaki"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.7" % "test"
)

initialCommands := "import com.qtamaki.lh._"

EclipseKeys.withSource := true


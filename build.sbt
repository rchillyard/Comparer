organization := "com.phasmidsoftware"

name := "Comparer"

version := "1.1.0"

val scala3Version = "3.3.8"
val scala213Version = "2.13.18"

scalaVersion := scala3Version

// Cross-build so that existing Scala 2.13 users can still depend on Comparer.
crossScalaVersions := Seq(scala3Version, scala213Version)

scalacOptions += "-deprecation"

val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

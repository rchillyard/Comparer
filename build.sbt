/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

organization := "com.phasmidsoftware"

name := "Comparer"

version := "2.0.0"

// Build with the oldest supported Scala 3 LTS so that the artifact is usable from any later Scala 3 version.
// CircleCI also runs the tests on the current LTS (3.9).
scalaVersion := "3.3.8"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all"
)

val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

organization := "com.phasmidsoftware"

name := "Comparer"

version := "2.1.0"

// The current Scala 3 LTS. Code built with 3.9 can only be used from Scala 3.9 or later.
scalaVersion := "3.9.0"

// The compiler options that all the Phasmid libraries share.
scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement"
)
// Tests may use a value as a statement (for example, in a matcher), so they skip -Wnonunit-statement.
Test / scalacOptions ~= (_.filterNot(_ == "-Wnonunit-statement"))

val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)


// Scaladoc is published to https://rchillyard.github.io/Comparer/latest/api/ by `sbt ghpagesPushSite`
enablePlugins(GhpagesPlugin, SiteScaladocPlugin)
git.remoteRepo := "git@github.com:rchillyard/Comparer.git"

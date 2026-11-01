/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

organization := "com.phasmidsoftware"

name := "Comparer"

version := "2.0.0"

// The current Scala 3 LTS. Code built with 3.9 can only be used from Scala 3.9 or later.
scalaVersion := "3.9.0"

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

// JUnit-style reports, collected by CircleCI
Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/test-reports")

// Scaladoc is published to https://rchillyard.github.io/Comparer/latest/api/ by `sbt ghpagesPushSite`
enablePlugins(GhpagesPlugin, SiteScaladocPlugin)
git.remoteRepo := "git@github.com:rchillyard/Comparer.git"

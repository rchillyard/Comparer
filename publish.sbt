/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

ThisBuild / organizationName := "Phasmid Software"
ThisBuild / organizationHomepage := Some(url("https://phasmidsoftware.com/"))

ThisBuild / description := "This project is a Scala library to facilitate comparisons in a functional style."
ThisBuild / homepage := Some(url("https://github.com/rchillyard/Comparer"))

// Publishing configuration (Sonatype Central Portal)
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
ThisBuild / publishMavenStyle := true

// Credentials
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")

// Required POM metadata for Maven Central
ThisBuild / licenses := List("GPLv3" -> url("https://gnu.org/licenses/gpl.html"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rchillyard/Comparer"),
    "scm:git@github.com:rchillyard/Comparer.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "rchillyard",
    name  = "Robin Hillyard",
    email = "rchillyard@phasmidsoftware.com",
    url   = url("https://phasmidsoftware.com")
  )
)

// Publishing settings
ThisBuild / versionScheme := Some("early-semver")

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

// sbt's lint check can't see that publishing reads these through ThisBuild, so it reports them as unused.
Global / excludeLintKeys ++= Set(pomIncludeRepository, publishMavenStyle)

name := """lucene-normalisinganalyzer"""

organization := "io.github.hsci-r"

version := "1.1.0"

scalaVersion := "2.13.6"

crossScalaVersions := Seq("2.11.12","2.12.10")

javacOptions ++= Seq("-source", "15", "-target", "15")

scalacOptions += "-target:jvm-15"

resolvers ++= Seq(
    Resolver.mavenLocal
)

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-analyzers-common" % "8.9.0",
  "org.apache.lucene" % "lucene-core" % "8.9.0",
  "junit" % "junit" % "4.13.2" % "test",
)

licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

publishMavenStyle := true

import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("hsci-r", "octavo-indexer", "eetu.makela@helsinki.fi"))

publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"



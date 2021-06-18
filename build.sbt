name := """lucene-normalisinganalyzer"""

organization := "fi.hsci"

version := "1.1.0"

scalaVersion := "2.13.6"

crossScalaVersions := Seq("2.11.12","2.12.10")

resolvers ++= Seq(
    Resolver.mavenLocal
)

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-analyzers-common" % "8.9.0",
  "org.apache.lucene" % "lucene-core" % "8.9.0",
  "junit" % "junit" % "4.13.2" % "test",
)

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

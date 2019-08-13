name := """lucene-normalisinganalyzer"""

organization := "fi.hsci"

version := "1.0.0"

scalaVersion := "2.12.9"

crossScalaVersions := Seq("2.11.12")

resolvers ++= Seq(
    Resolver.mavenLocal
)

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-analyzers-common" % "8.2.0",
  "org.apache.lucene" % "lucene-core" % "8.2.0",
  "org.apache.lucene" % "lucene-queryparser" % "8.2.0",
  "junit" % "junit" % "4.12" % "test",
)

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

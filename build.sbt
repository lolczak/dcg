name := "dcg"

organization := "org.lolczak"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "Tim Tennant's repo" at "http://dl.bintray.com/timt/repo/"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

//addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.2" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test" withSources() withJavadoc(),
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.chuusai" %% "shapeless" % "2.1.0" withSources() withJavadoc(),
  "org.scalaz" %% "scalaz-core" % "7.1.2" withSources() withJavadoc(),
  "org.scalaz" %% "scalaz-effect" % "7.1.0" withSources() withJavadoc(),
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  "org.codehaus.groovy" % "groovy" % "2.4.4"
)
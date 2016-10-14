name := "TaigiUtils"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

organization := "fr.magistry"


resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "fr.magistry" %% "nlplib" % "1.0-SNAPSHOT",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
//  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
//  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "org.xerial" % "sqlite-jdbc" % "3.8.11.2"
//  "org.deeplearning4j" % "deeplearning4j-nlp" % "0.5.0",
//  "org.nd4j" % "nd4j-native" % "0.5.0" classifier "" classifier "linux-x86_64",
//  "org.nd4j" % "nd4j" % "0.5.0",
//  "org.deeplearning4j" % "deeplearning4j-ui" % "0.5.0"
)

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

scalacOptions += "-target:jvm-1.7"
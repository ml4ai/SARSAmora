
name := "SARSAmora"
scalaVersion := "2.11.8"
version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  "jline" % "jline" % "2.12.1",
  "org.json4s" %% "json4s-native" % "3.5.1",
  // logging
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %%  "scala-logging" % "3.4.0",
  "org.scala-graph" %% "graph-core" % "1.11.3",
  "org.scalanlp" %% "breeze" % "0.13",
  "org.scalanlp" %% "breeze-natives" % "0.13",
  "org.scalanlp" %% "breeze-viz" % "0.13",
  "org.jfree" % "jfreechart" % "1.0.19"
)

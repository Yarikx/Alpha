name := "AlphaProcedure"

version := "1.0.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
        "com.github.wookietreiber" %% "scala-chart" % "latest.integration"  withSources() withJavadoc(),
        "org.scala-lang" % "scala-swing" % "2.10.2"
        )


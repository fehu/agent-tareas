import sbt._
import Keys._

import org.sbtidea.SbtIdeaPlugin._

object AgentosTarea1 extends Build {

  val ScalaVersion = "2.10.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := "feh.tec.agentos",
    version      := "0.1-SNAPSHOT",
    scalaVersion := ScalaVersion,
//    scalacOptions ++= Seq("-explaintypes"),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-debug")
  )

  lazy val lwjglSettings = Nicol.nicolSettings ++ Seq(
    LWJGLPlugin.lwjgl.version := "2.9.0"
  )

  object Dependencies{
    val akka = "com.typesafe.akka" %% "akka-actor" % "2.2.0"
  }

  import Dependencies._

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings
  ).settings(ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil)
   .dependsOn(agent, world, drawApi, lwjglVisualization)


  lazy val agent = Project(
    id = "agent",
    base = file("agent"),
    settings = buildSettings
  )

  //  lazy val environment = Project(
  //    id = "world",
  //    base = file("world"),
  //    settings = buildSettings
  //  )

  lazy val world = Project(
    id = "world",
    base = file("world"),
    settings = buildSettings
  ) dependsOn agent

  lazy val drawApi = Project(
    id = "draw-api",
    base = file("draw-api"),
    settings = buildSettings
  ) dependsOn world

  lazy val lwjglVisualization = Project(
    id = "lwjgl",
    base = file("lwjgl"),
    settings = buildSettings ++ LWJGLPlugin.lwjglSettings ++ Seq(
      libraryDependencies += akka
    )
  ) dependsOn drawApi
}
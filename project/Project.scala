import org.sbtidea.SbtIdeaPlugin._
import sbt._
import sbt.Keys._
import sbt.Keys.settings
import sbt.ScalaVersion

object AgentosTarea1 extends Build{
  val ScalaVersion = "2.10.2"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    name := "tarea-1",
    organization := "feh.tec.agentos",
    version := "0.1",
    scalaVersion := ScalaVersion
  )

  lazy val agentosTarea1 = Project(
    id = "agentos-tarea-1",
    base = file("."),
    settings = buildSettings
  ).settings(ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil)
   //.aggregate (world, lwjglVisualization)

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

  lazy val lwjglVisualization = Project(
    id = "lwjgl",
    base = file("lwjgl"),
    settings = buildSettings ++ LWJGLPlugin.lwjglSettings
  ) //dependsOn world
}
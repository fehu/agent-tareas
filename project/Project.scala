import sbt._
import Keys._
import sbtunidoc.Plugin._
import UnidocKeys._
import org.sbtidea.SbtIdeaPlugin._

object AgentosTarea1 extends Build {

  val ScalaVersion = "2.10.3"

  import Resolvers._
  import Dependencies._

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := "feh.tec.agentes",
    version      := "0.2.1",
    scalaVersion := ScalaVersion,
//    scalacOptions ++= Seq("-explaintypes"),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-debug"),
    resolvers += Release.spray
  )

  lazy val lwjglSettings = buildSettings ++ LWJGLPlugin.lwjglSettings /*Nicol.nicolSettings*/ ++ Seq(
    LWJGLPlugin.lwjgl.version := "2.9.0"
  )

  lazy val testsSettings = buildSettings ++ Seq(
    resolvers ++= Seq(Release.sonatype, Snapshot.sonatype),
    libraryDependencies ++= Seq(Tests.scalaCheck, Tests.specs2)
  )

  object Resolvers{
    object Release{
      val sonatype = "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
      val spray = "spray" at "http://repo.spray.io/"
    }

    object Snapshot{
      val sonatype = "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
    }

  }

  object Dependencies{
    lazy val akka = "com.typesafe.akka" %% "akka-actor" % "2.2.1"
    lazy val reflectApi = "org.scala-lang" % "scala-reflect" % ScalaVersion
    lazy val scalaSwing = "org.scala-lang" % "scala-swing" % ScalaVersion
    lazy val shapeless = "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1"

    object spray{
      lazy val json = "io.spray" %%  "spray-json" % "1.2.5"
    }

    object Tests{
      lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
      lazy val specs2 = "org.specs2" %% "specs2" % "2.2.2" % "test"
    }
  }

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ unidocSettings ++ lwjglSettings ++ Seq(
      mainClass in (Compile, run) := Some("feh.tec.agentes.tarea1.Tarea1Application")
    )
  ).settings(ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil)
   .dependsOn(tarea1)
   .aggregate(agent, lwjglVisualization, swingVisualization, drawIntegration, tarea1, tarea3)


  lazy val tarea1 =  Project(
    id = "tarea1",
    base = file("tarea1"),
    settings = testsSettings ++ Seq(
      libraryDependencies += spray.json
    )
    ++ lwjglSettings
  ) dependsOn (agent, lwjglVisualization, swingVisualization, drawIntegration)

  lazy val tarea3 =  Project(
    id = "tarea3",
    base = file("tarea3"),
    settings = buildSettings
  ) dependsOn agent

  lazy val agent = Project(
    id = "agent",
    base = file("agent"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(akka, reflectApi)
    )
  )

  lazy val drawIntegration = Project(
    id = "draw-integration",
    base = file("draw-integration"),
    settings = buildSettings
  ) dependsOn (agent, lwjglVisualization, swingVisualization)

  lazy val lwjglVisualization = Project(
    id = "lwjgl",
    base = file("lwjgl"),
    settings = lwjglSettings
  ) dependsOn agent

  lazy val swingVisualization = Project(
    id = "swing",
    base = file("swing"),
    settings = buildSettings ++ Seq(
      libraryDependencies += scalaSwing
    )
  ) dependsOn agent
}
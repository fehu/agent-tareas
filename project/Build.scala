import java.io.File
import LWJGLPlugin.lwjgl
import sbt._
import Keys._
import sbt.std.Streams
import sbtassembly.Plugin
import sbtassembly.Plugin.AssemblyKeys._
import sbtassembly.Plugin.{PathList, MergeStrategy, MappingSet}
import sbtunidoc.Plugin._
import org.sbtidea.SbtIdeaPlugin._

object Build extends sbt.Build {

  val ScalaVersion = "2.10.3"
  val Version = "0.3"

  val runPlugHole = InputKey[Unit]("run-plug-hole", "[Tarea1] Runs Plug-Hole Agent Application")
  val runPrisonerDilemma = InputKey[Unit]("run-prisoner-dilemma", "[Tarea3] Runs Prisoner Dilemma Game")

  val runResourceMapperWrite = TaskKey[Unit]("run-resource-mapper-write")
  val execResourceMapperWrite = TaskKey[Unit]("exec-resource-mapper-write")
  val copyAllLwjglResources = TaskKey[Unit]("copy-all-lwjgl-resources", "copy lwjgl resources for all platforms")
  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  import Resolvers._
  import Dependencies._

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := "feh.tec.agentes",
    version      := Version,
    scalaVersion := ScalaVersion,
//    scalacOptions ++= Seq("-explaintypes"),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-debug"),
    resolvers += Release.spray,
    mainClass in Compile := Some("feh.tec.agent.run.AgentApps")
  )

  lazy val lwjglSettings = buildSettings ++ LWJGLPlugin.lwjglSettings /*Nicol.nicolSettings*/ ++ Seq(
    LWJGLPlugin.lwjgl.version := "2.9.0",
    LWJGLPlugin.lwjgl.os <<= LWJGLPlugin.lwjgl.os {
      case ("macosx", "lib") => "osx" -> "jnilib"
      case other => other
    }
  )

  lazy val testsSettings = buildSettings ++ Seq(
    resolvers ++= Seq(Release.sonatype, Snapshot.sonatype),
    libraryDependencies ++= Seq(Tests.scalaCheck, Tests.specs2)
  )

  lazy val buildExecutableSettings = Plugin.assemblySettings ++ Seq(
    jarName in assembly := s"run-agents_$ScalaVersion-$Version.jar",
    outputPath in assembly <<= (baseDirectory in Compile, jarName in assembly) map {
      (base , jar) =>
        val dir = base / "dist"
        if(dir.exists()) IO.delete(dir)
        IO.createDirectory(dir)
        dir / jar
    },
    mergeStrategy in assembly <<= (mergeStrategy in assembly){ old =>
      val unwanted = List(".dll", ".so", ".jnilib", ".dylib")

    {
      case PathList(file) if unwanted.exists(file.endsWith) => MergeStrategy.discard
      case x => old(x)
    }},

    unmanagedResourceDirectories in Compile <+= baseDirectory (_ / "target" / "scala-2.10" / "resource_managed" / "main"),
    assembly <<= assembly.dependsOn(copyAllLwjglResources, runResourceMapperWrite) ,
    copyAllLwjglResources <<= (state, lwjgl.nativesDir, streams) map {
      (state, natives, s) =>
        s.log.info("copying lwjgl resources for all platforms")
//      val oldOs = sys.props("os")
      for{
        (os, ext) <- List("linux"->"so", "osx"->"jnilib", "windows"->"dll")
//          arch <- List("64", "32")
        } yield {
//          sys.props("os.arch") = arch
          val extracted = Project.extract(state)
          val setOs = lwjgl.os := os -> ext
          Project.runTask(lwjgl.copyNatives, extracted.append(List(setOs), state))
        }
//      sys.props("os.name") = oldOs
    },
    runResourceMapperWrite <<=  (lwjgl.copyDir, streams, dependencyClasspath in Runtime, runner) map {
      (dir, s, cp, run) =>
        val p = dir.absolutePath.split(File.separatorChar)
        val (baseSeq, glSeq) = p.toSeq.splitAt(p.size - 1)
        val base = baseSeq.mkString(File.separator)
        val gl = glSeq.head
        s.log.debug(s"generating resource mapper: $base -> $gl")
        Run.run("feh.tec.util.build.JsonResourceMapperExecutable", cp.files, base :: gl :: Nil, s.log)(run)
    },
    mainClass in assembly <<= (mainClass in Compile)
  )

  def myTask = Def.task {
    runTask(Compile, "feh.tec.util.build.JsonResourceMapperExecutable?")
  }

  lazy val runTasks = runPlugHoleTask :: runPrisonerDilemmaTask :: Nil
  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

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

    object Apache{
      lazy val ioCommons = "commons-io" % "commons-io" % "2.4"
    }

    object spray{
      lazy val json = "io.spray" %%  "spray-json" % "1.2.5"
    }

    object Tests{
      lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
      lazy val specs2 = "org.specs2" %% "specs2" % "2.2.2" % "test"
    }
  }

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ unidocSettings ++ lwjglSettings ++ runTasks ++ buildExecutableSettings
  ).settings(ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil)
   .dependsOn(tarea1, tarea3)
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
    settings = buildSettings ++ Seq(
      initialCommands +=
        """
          |import feh.tec.tarea3._
          |import scala.swing._
          |import Swing._
          |import java.awt.Color
          |import feh.tec.util._
          |val app = PrisonerDilemmaTest.app
        """.stripMargin
    )
  ) dependsOn (agent, swingVisualization)

  lazy val agent = Project(
    id = "agent",
    base = file("agent"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(akka, reflectApi, Apache.ioCommons)
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
    settings = lwjglSettings ++ Seq{
      libraryDependencies += spray.json
    }
  ) dependsOn agent

  lazy val swingVisualization = Project(
    id = "swing",
    base = file("swing"),
    settings = buildSettings ++ Seq(
      libraryDependencies += scalaSwing
    )
  ) dependsOn agent

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  lazy val runPlugHoleTask = fullRunInputTask(runPlugHole, Runtime, "feh.tec.agentes.tarea1.Tarea1Application")
  lazy val runPrisonerDilemmaTask = fullRunInputTask(runPrisonerDilemma, Runtime, "feh.tec.tarea3.PrisonerDilemmaTestApplication")
}
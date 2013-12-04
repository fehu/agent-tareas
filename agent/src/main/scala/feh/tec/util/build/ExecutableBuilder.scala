package feh.tec.util.build

import feh.tec.util.FileUtils
import scala.util.Try
import java.io.File

trait ExecutableBuilder extends FileUtils{
  def scriptFile(jar: Path): Try[File]
}

object ExecutableBuilderApp extends App {
  val builder = sys.props.get("os.name") match{
    case Some(str) if str.toLowerCase.contains("linux") => new BashExecutableBuilder
  }

  println{
    builder.scriptFile(builder.RelativePath.raw(args.toSeq))
  }
}

class BashExecutableBuilder extends ExecutableBuilder{
  def scriptFile(jar: Path): Try[File] = {
    val filename = jar.reversed.head
    val scr = dropExt(filename) + ".sh"
    val f = file(jar.back / scr)
    val txt = scriptText(filename)
    Try{
      f.withOutputStream(_.write(txt.getBytes("UTF-8")))
      f
    }
  }

  def scriptText(jar: String) =
    s"""
      |#!/bin/sh
      |#
      |  java -jar $jar
      |
    """.stripMargin
}
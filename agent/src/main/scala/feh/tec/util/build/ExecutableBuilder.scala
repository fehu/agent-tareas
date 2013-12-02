package feh.tec.util.build

import feh.tec.util.FileUtils
import scala.util.Try

trait ExecutableBuilder extends FileUtils{
  def scriptFile(jar: Path): Try[JFile]
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
  def scriptFile(jar: Path): Try[JFile] = {
    val filename = jar.reversedPath.head
    val scr = dropExt(filename) + ".sh"
    val f = file(jar.back / scr)
    val txt = scriptText(filename)
    Try{
      f.withOutputStream(_.write(txt.getBytes("UTF-8"))).file
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
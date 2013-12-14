#!/bin/sh
  SCRIPT="$(cd "${0%/*}" 2>/dev/null; echo "$PWD"/"${0##*/}")"
DIR=`dirname "${SCRIPT}"}`
exec scala $0 $DIR $SCRIPT
::!#

/** from http://timperrett.com/2011/08/01/system-scripting-with-scala/ */

import scala.xml._

val xmlPath = ".idea_modules/root.iml"
val brokenUrl = "file://$MODULE_DIR$/../"

val xml = XML.loadFile(xmlPath)

//val urls = xml \\ "sourceFolder" map { k => k -> k \ "@url" }
//val brk = urls.map(k => k -> k._2.headOption.map(_.text))
//val brokenOpt = brk.find(_._2 == Some(brokenUrl)).map(_._1._1)

def remove(node: NodeSeq): NodeSeq = node match {
  case elem @ Elem(_, "sourceFolder", meta, _, child @ _*) if meta.get("url").map(_.text) == Some(brokenUrl) =>
    println("removed broken")
    Nil
  case elem @ Elem(_, _, _, _, child @ _*) => elem.asInstanceOf[Elem].copy(child = child flatMap remove)
  case other => other
}

val newVersion = remove(xml)
XML.save(xmlPath, newVersion.asInstanceOf[Node])


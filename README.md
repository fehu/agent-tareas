Agents Project
========================

<i>An agents project for master studies in tec de monterrey</i>

To build and run the project one would need the <a href="http://www.scala-sbt.org/">sbt</a> building tool

One-file executable <a href="https://drive.google.com/file/d/0B9XpukXOfywNeDBHUmFzZ3V0aGM/edit?usp=sharing">jar</a>, due to it's almost 20Mb size it already has all dependencies
<br/>In order to run it simply execute <code>java -jar run-agents_...jar</code>

* Execute <code>sbt update</code> in project directory to fetch the library dependencies 
* To run tests run <code>sbt test</code> <i><b>#</b> running tests from sbt temporary broken <b>#</b></i>
* Execute <code>sbt run</code> to run a launcher
* To build a one-jar app, run <code>sbt assembly</code> <b>twice</b>
* To run the 'plug-hole' application run <code>sbt run-plug-hole</code>
  * To run with agent execution frequency specified, run <code> sbt "run-plug-hole execution_frequency"</code>
    where <i>execution_frequency</i> is a string, composed of duration and unit, for example:
    1 sec, 0.5 sec, 200 millis
  * The application is stopped by 'Escape' and paused/resumed by 'Space' keys
   <br/><i><b>#</b> pause/resume temporary doesn't work if launched from Apps Runner <i><b>#</b>
* To run prisoner dilemma game, run <code>sbt run-prisoner-dilemma</code>

IDE Integration
* <a href="http://www.jetbrains.com/idea/">IntelliJ IDEA</a> (There is a free opensource community version) <br/> 
  * Execute <code>sbt gen-idea</code> to generate a project layout
  * Install scala plugin from idea settings -> plugins -> browse repositories
  * To run a lwjgl frontend application via idea, add a VM option: <br/><code>-Dorg.lwjgl.librarypath=<br/>$PROJECT_DIR$/lwjgl/target/scala-2.10/resource_managed/main/lwjgl-resources/<i>%PLATFORM%</i></code>
    <br/>where <code><i>%PLATFORM%</i></code> is one of following: <code>linux</code> / <code>macosx</code> / <code>windows</code>
  * if you see some "Output path ... intersects with a source root. Only files that were created by build will be cleaned." messages,
     followed by a lot of errors, run <code>scala fix-iml.sc</code> or <code>sh fix-iml.sc</code>
* For Eclipse integration see <a href="http://scala-ide.org/">ScalaIDE<a/> project

Useful links 
* Scala for Java programmers (<a href='http://docs.scala-lang.org/es/tutorials/scala-for-java-programmers.html'>Español<a/>)
* <a href="http://www.cs.ucsb.edu/~benh/162/Programming-in-Scala.pdf">Programming in Scala</a> book by Martin Odersky

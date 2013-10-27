Agentes - Tarea №1
========================

<i>An agents project for master studies in tec de monterrey</i>

To build and run the project one would need the <a href="http://www.scala-sbt.org/">sbt</a> building tool

* Execute <code>sbt update</code> in project directory to fetch the library dependencies 
* To run tests execute <code>sbt test</code>
* To run the application execute <code>sbt run</code>
  * To run with agent execution frequency specified run <code> sbt "run execution_frequency"</code>
    where <i>execution_frequency</i> is a string, composed of duration and unit, for example:
      1 sec      0.5 sec      200 millis
  * The application is stopped by 'Escape' and paused/resumed by 'Space' keys

IDE Integration
* <a href="http://www.jetbrains.com/idea/">IntelliJ IDEA</a> (There is a free opensource community version) <br/> 
  * Execute <code>sbt gen-idea</code> to generate a project layout
  * Install scala plugin from idea settings -> plugins -> browse repositories
* For Eclipse integration see <a href="http://scala-ide.org/">ScalaIDE<a/> project

Useful links 
* Scala for Java programmers (<a href='http://docs.scala-lang.org/es/tutorials/scala-for-java-programmers.html'>Español<a/>)

package feh.tec.agentes.tarea1

import org.scalacheck.{Properties, Prop, Gen, Arbitrary}
import akka.actor.ActorSystem
import feh.tec.agent.AgentId
import org.specs2.specification.Example
import scala.concurrent.duration._
import feh.tec.agentes.tarea1.Tarea1.{Environment => Env}
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.execute.Result
import org.specs2.matcher.MatchResult


trait Arbitraries{ self: Specification with ScalaCheck =>
  import Arbitrary._
  import Conf._

  implicit def environment: Arbitrary[Environment] = Arbitrary{
    for { x <- Gen.chooseNum(10, 30); y <- Gen.chooseNum(5, 15) }
    yield new Environment(
      Maps.randomMap(0 until x, 0 until y, Option(agentId)),
      Env.effects,
      Env.initGlobal,
      Env.mapStateBuilder
    )
  }

  implicit def overseer: Arbitrary[Overseer] = Arbitrary{
    for(env <- arbitrary[Environment]) yield Tarea1.overseer(env, timeouts)
  }

  implicit def envRef: Arbitrary[Environment#Ref] = Arbitrary{ for(ov <- arbitrary[Overseer]) yield ov.ref }

  implicit def envSnapshot: Arbitrary[Environment#Snapshot] = Arbitrary{ for( ref <- arbitrary[Environment#Ref]) yield ref.blocking.snapshot }

  implicit def exampleToProp(e: => Example): Prop = new Properties(e.desc.toString()){
    e.examples.map{
      ex => property(ex.desc.toString()) = {
        val res = ex.execute
        if(!res.isSuccess) Console.err.println("error in '" + ex.desc.toString() + "': " + res.message)
        res
      }
    }
  }

  implicit class ResultItWrapper(results: Iterable[Result]){
    def all = results reduceLeft (_ and _)
    def atLeastOne = results reduceLeft (_ or _)
  }

  implicit class MatchResultItWrapper(results: Iterable[MatchResult[Any]]){
    def all = results reduceLeft (_ and _)
    def atLeastOne = results reduceLeft (_ or _)
  }

  implicit class PropItWrapper(results: Iterable[Prop]){
    def all = results reduceLeft (_ && _)
    def atLeastOne = results reduceLeft (_ || _)
  }
}

object Conf{
  implicit def actorSystem = ActorSystem()
  def timeouts = OverseerTimeouts(
    defaultBlockingTimeout = 10,
    defaultFutureTimeout = 10,
    predictMaxDelay =  100 milli,
    foreseeMaxDelay = 100 milli,
    getMapMaxDelay =  10 milli,
    positionMaxDelay =  30 milli
  )

  lazy val agentId = AgentId()

}

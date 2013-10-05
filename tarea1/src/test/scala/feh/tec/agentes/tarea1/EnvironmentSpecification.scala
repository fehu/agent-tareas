package feh.tec.agentes.tarea1

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.{Properties, Prop, Gen, Arbitrary}
import feh.tec.agentes.tarea1.Tarea1.{Environment => Env}
import feh.tec.util._
import akka.actor.ActorSystem
import feh.tec.visual.api.SquareMapDrawOptions
import feh.tec.visual.NicolLike2DEasel
import scala.concurrent.duration._
import feh.tec.agentes.tarea1.DummyMapGenerator.DummyMapGeneratorRandomPositionSelectHelper
import scala.util.Random
import feh.tec.agent.AgentId
import org.specs2.specification.Example
import feh.tec.agentes.tarea1.test.NicolLikeMapVisualizer
import feh.tec.util.FileUtils.ByteArrayToFileWrapper

class EnvironmentSpecification extends Specification with ScalaCheck{
  import Arbitraries._
  import Conf._

  "The Environment" should{
    "be accessible at all coordinates defined" in prop{ env: Environment => env.definedAt forall env.get.isDefinedAt }
    "contain agent's avatar" in prop{ref: Environment#Ref => ref.position(agentId) must beSome}
    "respond to actions: " in prop {
      (overseer: Overseer) =>
        val ref = overseer.ref
        val env = overseer.env

        val visual = new NicolLikeMapVisualizer(env)

        def pos = ref.position(agentId).get
        def passOpt(pos: (Int, Int)) = if(ref.blocking.stateOf(pos).exists(_.hole)) None else Some(pos)
        val iPos = pos
        val northPos = passOpt(iPos._1 -> (if(iPos._2  == 0) env.coordinates.yRange.max else iPos._2 - 1)) getOrElse iPos
        val eastPos = passOpt((if(northPos._1 == env.coordinates.xRange.max) 0 else northPos._1 + 1) -> northPos._2) getOrElse northPos
        val southPos = passOpt(eastPos._1 -> (if(eastPos._2 == env.coordinates.yRange.max) 0 else eastPos._2 + 1)) getOrElse eastPos
        val westPos = passOpt((if(southPos._1 == 0) env.coordinates.xRange.max else southPos._1 - 1) -> southPos._2) getOrElse southPos

        println(s"positions: init=$iPos, north=$northPos, east=$eastPos, south=$southPos, west=$westPos")

//        def screenshot(file: String) = visual.game.screenshot().toFile(file + ".bmp")

        visual.show()
        sequential
//        screenshot("0-init")
        "move north" >> { ref.blocking.affect(MoveNorth); pos mustEqual northPos }
//        screenshot("1-north")
        "move east"  >> { ref.blocking.affect(MoveEast); pos mustEqual eastPos }
//        screenshot("2-east")
        "move south" >> { ref.blocking.affect(MoveSouth); pos mustEqual southPos }
//        screenshot("3-south")
        "move west"  >> { ref.blocking.affect(MoveWest); pos mustEqual westPos }
//        screenshot("4-west")

        visual.close()
    }
  }

  "Environment snapshots" should{
    "be accessible at all coordinates defined" in
      prop{ s: Environment#Snapshot =>  s.definedAt forall s.asEnv.get.isDefinedAt }
  }

  object Arbitraries{
    import Arbitrary._

    implicit def environment: Arbitrary[Environment] = Arbitrary{
      for { x <- Gen.chooseNum(10, 100); y <- Gen.chooseNum(10, 100) }
      yield new Environment(
        DummyMapGenerator.withHelpers[DummyMapGeneratorRandomPositionSelectHelper]
          .buildTilesMap(1 to x, 1 to y)(h => (x, y) => PartialFunction.condOpt(Random.nextDouble()) {
            case _ if h.uniqueRandomPosition == x -> y => AgentAvatar(agentId)
            case r if r > 0.8 => Hole()
            case r if r < 0.2 => Plug()
        }) andThen (_.values.toSeq),
        1 to x,
        1 to y,
        Env.effects,
        Env.initGlobal,
        Env.mapStateBuilder
      )
    }

    implicit def overseer: Arbitrary[Overseer] = Arbitrary{
      for(env <- arbitrary[Environment]) yield Tarea1.overseer(env, timeouts, mapRenderer, easel, mapDrawConfig)
    }

    implicit def envRef: Arbitrary[Environment#Ref] = Arbitrary{ for(ov <- arbitrary[Overseer]) yield ov.ref }

    implicit def envSnapshot: Arbitrary[Environment#Snapshot] = Arbitrary{ for( ref <- arbitrary[Environment#Ref]) yield ref.blocking.snapshot }
  }

  object Conf{
    implicit def actorSystem = ActorSystem()
    def mapRenderer = LwjglTest.createMapRenderer
    def easel = LwjglTest.createEasel
    def mapDrawConfig = new SquareMapDrawOptions[NicolLike2DEasel]{
      def tileSideSize: NicolLike2DEasel#CoordinateUnit = 50
    }
    def timeouts = OverseerTimeouts(
      defaultBlockingTimeout = 10,
      defaultFutureTimeout = 10,
      predictMaxDelay =  100 milli,
      foreseeMaxDelay = 100 milli,
      getMapMaxDelay =  10 milli,
      positionMaxDelay =  30 milli
    )

    lazy val agentId = AgentId()

    implicit def exampleToProp(e: => Example): Prop = new Properties("*"){
      property(e.desc.toString()) = e.execute
    }

  }
}

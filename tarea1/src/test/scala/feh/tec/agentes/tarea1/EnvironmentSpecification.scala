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
import feh.tec.util.FileUtils.ByteArrayToFileWrapper
import java.nio.charset.Charset
import java.io.File
import org.scalacheck.Prop.False

class EnvironmentSpecification extends Specification with ScalaCheck{
  import Arbitraries._
  import Conf._

  createTestsFolder()

  "The Environment" should{
    "be accessible at all coordinates defined" in prop{ env: Environment => env.definedAt forall env.get.isDefinedAt }
    "contain agent's avatar" in prop{ref: Environment#Ref => ref.position(agentId) must beSome}
    "provide sense information correctly" in prop{
      (overseer: Overseer) =>
        val ref = overseer.ref
        val env = overseer.env

        "about visible coordinates" >> { ref.blocking.visibleStates.keySet mustEqual env.definedAt.toSet } &&
        "about states at given coordinates" >> {
          val bulked = ref.blocking.visibleStates
          val resSeq = for(c <- bulked.keys; state = env.stateByTile(env.tilesAsMap(c))) yield
            ref.blocking.stateOf(c) must beSome(state) and bulked(c).mustEqual(state)
          resSeq.reduceLeft(_ and _)
        }

    }
    "respond to actions: " in prop {
      (overseer: Overseer) =>
        val ref = overseer.ref
        val env = overseer.env

        def pos = ref.position(agentId).get
        def passOpt(pos: (Int, Int)) = if(ref.blocking.stateOf(pos).exists(_.hole)) None else Some(pos)
        val iPos = pos
        val northPos = passOpt(iPos._1 -> (if(iPos._2  == env.coordinates.yRange.min) env.coordinates.yRange.max else iPos._2 - 1)) getOrElse iPos
        val eastPos = passOpt((if(northPos._1 == env.coordinates.xRange.max) env.coordinates.xRange.min else northPos._1 + 1) -> northPos._2) getOrElse northPos
        val southPos = passOpt(eastPos._1 -> (if(eastPos._2 == env.coordinates.yRange.max) env.coordinates.yRange.min else eastPos._2 + 1)) getOrElse eastPos
        val westPos = passOpt((if(southPos._1 == env.coordinates.xRange.min) env.coordinates.xRange.max else southPos._1 - 1) -> southPos._2) getOrElse southPos

        lazy val serializer = new MapJsonSerializer

        val pref = "tests" + File.separator

        s"positions: init=$iPos, north=$northPos, east=$eastPos, south=$southPos, west=$westPos".getBytes.toFile(pref + "positions")
        env.tiles.toSeq.mkString("\n").getBytes.toFile(pref + "init-map")

        def screenshot(file: String) = serializer.serialize(env).prettyPrint.getBytes.toFile(file)

        sequential
        screenshot(pref + "0-init")
        "move north" >> { ref.blocking.affect(MoveNorth); screenshot(pref + "1-north"); pos mustEqual northPos } &&
        "move east"  >> { ref.blocking.affect(MoveEast);  screenshot(pref + "2-east");  pos mustEqual eastPos  } &&
        "move south" >> { ref.blocking.affect(MoveSouth); screenshot(pref + "3-south"); pos mustEqual southPos } &&
        "move west"  >> { ref.blocking.affect(MoveWest);  screenshot(pref + "4-west");  pos mustEqual westPos  }
    }
  }

  "Environment snapshots" should{
    "be accessible at all coordinates defined" in
      prop{ s: Environment#Snapshot =>  s.definedAt forall s.asEnv.get.isDefinedAt }
  }

  object Arbitraries{
    import Arbitrary._

    implicit def environment: Arbitrary[Environment] = Arbitrary{
      for { x <- Gen.chooseNum(10, 30); y <- Gen.chooseNum(5, 15) }
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
      def showLabels: Boolean = true
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

    implicit def exampleToProp(e: => Example): Prop = new Properties(e.desc.toString()){
      e.examples.map{
        ex => property(ex.desc.toString()) = {
          val res = ex.execute
          if(!res.isSuccess) Console.err.println("error in '" + ex.desc.toString() + "': " + res.message)
          res
        }

      }

    }

  }

  def createTestsFolder() {
    val file = new File("tests")
    if(!file.exists()) file.mkdir()
  }
}

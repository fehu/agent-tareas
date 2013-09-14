package feh.tec.agentes.tarea1

import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.map.{TileSnapshot, MapSnapshot}
import feh.tec.util._
import feh.tec.agent.AgentId

object Criteria {
  trait PlugsMovingAgentCriteria extends Debugging{
    import Agent._

    def debugMessagePrefix: String = "[Criteria] "

    protected def criterion(assess: Measure#Snapshot => Measure#Measure) =
      Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure](assess)

    def toList: List[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]] = Nil
  }

  /**
   *  A criterion based on number of unplugged holes.
   */
  trait NumberOfHolesCriterion extends PlugsMovingAgentCriteria{

    /**
     * Should be negative.
     */
    def numberOfHolesWeight: Double

    def numberOfHoles = criterion{
      _.asEnv.tiles.count(_.contents.exists(_.isHole)) * numberOfHolesWeight debugLog "weighted number of holes"
    }

    override def toList = numberOfHoles :: super.toList
  }

  /**
   *  A criterion based on distances between two closest hole and plug
   *  the distance is calculated without considering MapObjs
   */
  trait ClosestPairIntraDistanceCriterion extends PlugsMovingAgentCriteria{
    import Agent._

    def closestPairIntraDistanceWeight: Int

    def closestPairIntraDistance = criterion{
      snapshot =>
        val mapSnapshot = Map.snapshotBuilder.snapshot(snapshot.asEnv)
        val pairs = findClosestHolePlugPairsWithIntraDistances(mapSnapshot)
        val minDist = pairs.minBy(_._3)._3

        minDist * closestPairIntraDistanceWeight debugLog "weighted closest hole-plug"

    }

    /**
     *  search is done first by hole and then it's closest plug, with possible repetitions
     */
    protected def findClosestHolePlugPairsWithIntraDistances(snapshot: MapSnapshot[Map, Tile, Position]): Seq[(Position, Position, Int)] =
      snapshot.tilesSnapshots
        .withFilter(_.asTile.contents.exists(_.isHole))
        .flatMap(hole =>
          findClosetDisregardingHoles(snapshot)(hole.coordinate, _.exists(_.isPlug))
            .map(x => (hole.coordinate, x._1, x._2))
          )

    protected def findClosetDisregardingHoles(snapshot: MapSnapshot[Map, SqTile, Position])
                                             (relativelyTo: Position, what: Option[MapObj] => Boolean): Option[(Position, Int)] = {
      var stopFlag = false

      def rec(t: TileSnapshot[Tile, Position], dist: Int): Option[(Position, Int)] = {
        if(stopFlag) return None

        val closestOpt = t.neighboursSnapshots.find(_.asTile.contents |> what)
        if(closestOpt.isEmpty) t.neighboursSnapshots. flatMap(rec(_, dist + 1)).headOption
        else {
          stopFlag = true
          closestOpt.map(_.coordinate -> dist)
        }
      }

      rec(snapshot.getSnapshot(relativelyTo), 0)
    }

    override def toList = closestPairIntraDistance :: super.toList
  }


  /**
   *  A criterion based on distance between the agent and closest plug that is a closest plug for some hole
   */
  trait DistanceToClosestPlugCriterion extends PlugsMovingAgentCriteria{
    self: ClosestPairIntraDistanceCriterion =>

    def agentId: AgentId

    def distanceToClosestPlug = criterion{
      snapshot =>
//        val closetHoles = findClosest(_.isHole, snapshot, snapshot.asEnv.agentsPositions(agentId))
//        val closestPlugs = closetHoles.map(hpos => findClosest(_.isPlug, snapshot, snapshot))

        0
    }

    protected def shortestRouteFinder: MapShortestRouteFinder

    override def toList = distanceToClosestPlug :: super.toList
  }
}

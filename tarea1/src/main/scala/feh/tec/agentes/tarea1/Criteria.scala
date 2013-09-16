package feh.tec.agentes.tarea1

import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.map.{MapEnvironmentSnapshot, TileSnapshot, MapSnapshot}
import feh.tec.util._
import feh.tec.agent.{EnvironmentSnapshot, AgentId}

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

    def closestHolePlugPairMeanIntraDistanceWeight: Float

    protected def guardCalculatedClosestHolePlugPairsWithIntraDistances(distMap: Predef.Map[Position, (Set[Position], Int)])

    def closestPairIntraDistance = criterion{
      snapshot =>
        val pairsWithDists = findClosestHolePlugPairsWithIntraDistances(
          snapshot.asInstanceOf[MapEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]].mapSnapshot
        )
        guardCalculatedClosestHolePlugPairsWithIntraDistances(pairsWithDists)
        val holesDists = pairsWithDists.map{
          case (h, (plugs, dist)) => h -> dist
        }
        val minDistsMean = holesDists.map(_._2).sum / holesDists.size

        minDistsMean * closestHolePlugPairMeanIntraDistanceWeight debugLog "weighted closest hole-plug distances sum"

    }

    /**
     *  search is done first by hole and then it's closest plug, with possible repetitions
     */
    protected def findClosestHolePlugPairsWithIntraDistances(snapshot: MapSnapshot[Map, Tile, Position]): Predef.Map[Position, (Set[Position], Int)] =
      snapshot.tilesSnapshots
        .withFilter(_.asTile.contents.exists(_.isHole))
        .map(hole =>
          hole.coordinate -> findClosetDisregardingHoles(snapshot)(hole.coordinate, _.exists(_.isPlug)))
        .toMap

    protected def findClosetDisregardingHoles(snapshot: MapSnapshot[Map, SqTile, Position])
                                             (relativelyTo: Position, what: Option[MapObj] => Boolean): (Set[Position], Int) = {
      var stopDist: Option[Int] = None

      def rec(t: TileSnapshot[Tile, Position], dist: Int): (Set[Position], Int) = {
        if(stopDist.exists(_ < dist)) return Set.empty[Position] -> dist

        val closestOpt = t.neighboursSnapshots.filter(_.asTile.contents |> what)
        if(closestOpt.isEmpty) t.neighboursSnapshots.flatMap(rec(_, dist + 1)._1).toSet -> dist
        else {
          stopDist = Some(dist)
          closestOpt.map(_.coordinate).toSet -> dist
        }
      }

      val res = rec(snapshot.getSnapshot(relativelyTo), 1)
      res
    }

    override def toList = closestPairIntraDistance :: super.toList
  }


  /**
   *  A criterion based on distance between the agent and closest plug that is a closest plug for some hole
   *  todo: very slow and breaks performance
   */
  @deprecated("broken")
  trait DistanceToClosestPlugCriterion extends PlugsMovingAgentCriteria{
    self: ClosestPairIntraDistanceCriterion =>

    import Agent._

    def agentId: AgentId

    def distanceToClosestPlugWeight: Float

    private var calculatedClosestHolePlugPairsWithIntraDistances: Option[Predef.Map[Position, (Set[Position], Int)]] = None
    protected def guardCalculatedClosestHolePlugPairsWithIntraDistances(distMap: Predef.Map[Position, (Set[Position], Int)]) =
      calculatedClosestHolePlugPairsWithIntraDistances = Option(distMap)
    protected def getCalculatedClosestHolePlugPairsWithIntraDistances = calculatedClosestHolePlugPairsWithIntraDistances
    protected def closestHolePlugPairsWithIntraDistances(mapSnapshot: MapSnapshot[Map, Tile, Position]) =
      getCalculatedClosestHolePlugPairsWithIntraDistances.getOrElse(findClosestHolePlugPairsWithIntraDistances(mapSnapshot))
    protected def discardCalculatedClosestHolePlugPairsWithIntraDistances() = calculatedClosestHolePlugPairsWithIntraDistances = None

    def distanceToClosestPlug = criterion{
      snapshot =>
        val mapSnapshot = snapshot.asInstanceOf[MapEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]].mapSnapshot // todo: casting!!!
        val agPos = snapshot.asEnv.agentsPositions(agentId).coordinate
        val holesClosestToAg = findClosetDisregardingHoles(mapSnapshot)(agPos, _.exists(_.isHole))
        val closestHolePlugPairs = closestHolePlugPairsWithIntraDistances(mapSnapshot)
        println(s"holesClosestToAg=$holesClosestToAg")
        println(s"closestHolePlugPairs=$closestHolePlugPairs")
        val plugsClosestToHoles = holesClosestToAg._1.flatMap(closestHolePlugPairs andThen (_._1))
        println(s"plugsClosestToHoles = $plugsClosestToHoles")

        val gr = shortestRouteFinder.mapAsGraph(mapSnapshot)
        val minDistMap = shortestRouteFinder.findMinimalDistances(gr)
        shortestRouteFinder.withMinDistMap(minDistMap, _.onGraph(gr){
          val plugsClosestToAg = shortestRouteFinder.findClosest(mapSnapshot)(agPos, Function untupled plugsClosestToHoles.contains _)
//          val plugsClosestToAgWithDistsFromTheirHoles todo implement
          val plugsClosestToAgDist =  plugsClosestToAg.headOption.map(_._2) getOrElse 0
//          discardCalculatedClosestHolePlugPairsWithIntraDistances()

          plugsClosestToAgDist * distanceToClosestPlugWeight debugLog "weighted distance to closest plug"
        })
    }

    protected def shortestRouteFinder: MapShortestRouteFinder

    override def toList = distanceToClosestPlug :: super.toList
  }
}

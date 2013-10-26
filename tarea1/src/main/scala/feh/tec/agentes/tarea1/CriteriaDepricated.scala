package feh.tec.agentes.tarea1

import feh.tec.map.{TileSnapshot, MapSnapshot, MapEnvironmentSnapshot}
import scala.collection.mutable
import feh.tec.agent.AgentId
import feh.tec.agentes.tarea1.Criteria.PlugsMovingAgentCriteria

/**
 *  Saved for history
 */
object CriteriaDepricated {

  /**
   *  A criterion based on distances between two closest hole and plug
   *  the distance is calculated without considering MapObjs
   */
  @deprecated("broken")
  trait ClosestPairIntraDistanceCriterion extends PlugsMovingAgentCriteria{
    import Agent._

    def closestHolePlugPairMeanIntraDistanceWeight: Float

    protected def guardCalculatedClosestHolePlugPairsWithIntraDistances(distMap: Predef.Map[Position, (Position, Int)])

    def closestPairIntraDistance = criterion("closest pair intra-distance", {
      snapshot =>
        val pairsWithDists = findClosestHolePlugPairsWithIntraDistances(
          snapshot.asInstanceOf[MapEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]].mapSnapshot
        )
        guardCalculatedClosestHolePlugPairsWithIntraDistances(pairsWithDists)
        val holesDists = pairsWithDists.map{
          case (h, (plugs, dist)) => h -> dist
        }
        val minDistsMean = if(holesDists.size == 0) 0 else holesDists.map(_._2).sum.toFloat / holesDists.size

        minDistsMean * closestHolePlugPairMeanIntraDistanceWeight debugLog "weighted closest hole-plug distances sum"

    })

    /**
     *  search is done first by hole and then it's closest plug, with possible repetitions
     */
    protected def findClosestHolePlugPairsWithIntraDistances(snapshot: MapSnapshot[Map, Tile, Position]): Predef.Map[Position, (Position, Int)] =
      snapshot.tilesSnapshots
        .withFilter(_.asTile.contents.exists(_.isHole))
        .flatMap(hole =>
        findClosetDisregardingHoles(snapshot)(hole.coordinate, _.exists(_.isPlug)).map(hole.coordinate ->))
        .toMap

    protected def findClosetDisregardingHoles(snapshot: MapSnapshot[Map, Tile, Position])
                                             (relativelyTo: Position, what: Option[MapObj] => Boolean): Option[(Position, Int)] = {
      val searchQueue = mutable.Queue.empty[(TileSnapshot[Tile, Position], Int)]
      val visited = mutable.HashSet.empty[Position]

      def search: Option[(TileSnapshot[Tile, Position], Int)] =
        if(searchQueue.nonEmpty) (doTheSearch _).tupled(searchQueue.dequeue()) orElse search
        else None

      def doTheSearch(sn: TileSnapshot[Tile, Position], d: Int): Option[(TileSnapshot[Tile, Position], Int)] = {
        visited += sn.coordinate
        if(what(sn.asTile.contents)) Some(sn -> d) else {
          searchQueue.enqueue(sn.neighboursSnapshots.withFilter(s => !visited.contains(s.coordinate)).map(_ -> (d+1)): _*)
          None
        }
      }

      searchQueue.enqueue(snapshot.getSnapshot(relativelyTo) -> 0)
      search.map{case (sn, d) => sn.coordinate -> d}
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

    private var calculatedClosestHolePlugPairsWithIntraDistances: Option[Predef.Map[Position, (Position, Int)]] = None
    protected def guardCalculatedClosestHolePlugPairsWithIntraDistances(distMap: Predef.Map[Position, (Position, Int)]) =
      calculatedClosestHolePlugPairsWithIntraDistances = Option(distMap)
    protected def getCalculatedClosestHolePlugPairsWithIntraDistances = calculatedClosestHolePlugPairsWithIntraDistances
    protected def closestHolePlugPairsWithIntraDistances(mapSnapshot: MapSnapshot[Map, Tile, Position]) =
      getCalculatedClosestHolePlugPairsWithIntraDistances.getOrElse(findClosestHolePlugPairsWithIntraDistances(mapSnapshot))
    protected def discardCalculatedClosestHolePlugPairsWithIntraDistances() = calculatedClosestHolePlugPairsWithIntraDistances = None

    def distanceToClosestPlug = criterion("distance to closest plug", {
      snapshot =>
        val mapSnapshot = snapshot.asInstanceOf[MapEnvironmentSnapshot[Map, Tile, Position, EnvState, EnvGlobal, Action, Env]].mapSnapshot // todo: casting!!!
      val agPos = snapshot.asEnv.agentsPositions(agentId).coordinate
        val holeClosestToAgOpt = findClosetDisregardingHoles(mapSnapshot)(agPos, _.exists(_.isHole))
        val closestHolePlugPairs = closestHolePlugPairsWithIntraDistances(mapSnapshot)
        println(s"holeClosestToAgOpt=$holeClosestToAgOpt")
        println(s"closestHolePlugPairs=$closestHolePlugPairs")
        val plugsClosestToHoleOpt = holeClosestToAgOpt.map(p => closestHolePlugPairs andThen (_._1) apply p._1)
        println(s"plugsClosestToHoles = $plugsClosestToHoleOpt")

        plugsClosestToHoleOpt.map{ pos =>
          val gr = Graph.mapAsGraph(mapSnapshot)
          val minDistMap = shortestRouteFinder.findMinimalDistances(gr)
          shortestRouteFinder.withMinDistMap(minDistMap, _.onGraph(gr){
            val plugsClosestToAg = shortestRouteFinder.findClosest(mapSnapshot)(agPos, pos == _-> _)
            val plugsClosestToAgDist =  plugsClosestToAg.headOption.map(_._2) getOrElse 0
            plugsClosestToAgDist * distanceToClosestPlugWeight.toDouble debugLog "weighted distance to closest plug"
          })
        } getOrElse 0.
    })

    protected def shortestRouteFinder: MapShortestRouteFinder

    override def toList = distanceToClosestPlug :: super.toList
  }

}

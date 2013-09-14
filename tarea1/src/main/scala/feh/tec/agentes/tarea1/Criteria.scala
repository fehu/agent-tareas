package feh.tec.agentes.tarea1

import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion

object Criteria {
  trait PlugsMovingAgentCriteria{
    import Agent._

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
      _.asEnv.tiles.count(_.contents.exists(_.isHole)) * numberOfHolesWeight
    }

    override def toList = numberOfHoles :: super.toList
  }

  /**
   *  A criterion based on distances between two closest hole and plug
   */
  trait ClosestPairIntraDistanceCriterion extends PlugsMovingAgentCriteria{
    import Agent._

    def closestPairIntraDistanceWeight: Int

    def closestPairIntraDistance = criterion{
      snapshot =>
        findClosestHolePlugPairs(snapshot)
          .map((shortestRouteFinder.shortestRoute(snapshot.asEnv) _).tupled).length * closestPairIntraDistanceWeight
    }

    protected def shortestRouteFinder: MapShortestRouteFinder

    /**
     *  searching is done first by hole and then it's closest plug, with possible repetitions
     */
    protected def findClosestHolePlugPairs(snapshot: Measure#Snapshot): Seq[(Position, Position)] =
      snapshot.asEnv.tiles
        .withFilter(_.contents.exists(_.isHole))
        .flatMap(tile => findClosest(_.isPlug, snapshot, tile).headOption.map(tile.coordinate ->))

    protected def findClosest(cond: MapObj => Boolean, snapshot: Measure#Snapshot, closestTo: Tile): Seq[Position] = {
      val neighbours = snapshot.asEnv.getNeighbors(closestTo)
      val closest = neighbours.filter(_.contents.exists(cond)).map(_.coordinate)
      if(closest.isEmpty) neighbours.flatMap(findClosest(cond, snapshot, _))
      else closest
    }

    override def toList = closestPairIntraDistance :: super.toList
  }


 /* /**
   * todo: A criterion based on distance between the agent and closest plug that is a closest plug for some hole
   */
  trait DistanceToClosestPlugCriterion extends PlugsMovingAgentCriteria{
    self: ClosestPairIntraDistanceCriterion =>

    def agentId: AgentId

    def distanceToClosestPlug = criterion{
      snapshot =>
        val closetHoles = findClosest(_.isHole, snapshot, snapshot.asEnv.agentsPositions(agentId))
        val closestPlugs = closetHoles.map(hpos => findClosest(_.isPlug, snapshot, snapshot))

        0
    }

    override def toList: List[C] = super.toList
  }*/
}

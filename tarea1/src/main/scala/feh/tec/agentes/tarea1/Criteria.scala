package feh.tec.agentes.tarea1

import feh.tec.util._
import feh.tec.agentes.tarea1.Agent._
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion
import feh.tec.agent.AgentId
import RandomWrappers._

object Criteria {
  trait PlugsMovingAgentCriteria extends Debugging{
    def debugMessagePrefix: String = "[Criteria] "

    protected def criterion(name: String, assess: Measure#Snapshot => Measure#Measure) =
      Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure](name, assess)

    def toList: List[Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure]] = Nil
  }

  /**
   *  A criterion based on number of unplugged holes.
   *  Is designed to be used with foreseeing agent.
   */
  trait NumberOfHolesCriterion extends PlugsMovingAgentCriteria{

    /**
     * Should be negative.
     */
    def numberOfHolesWeight: Double

    def numberOfHoles = criterion("number of holes", {
      s =>
        val n = s.asEnv.tiles.count(_.contents.exists(_.isHole))
        n * numberOfHolesWeight debugLog "weighted number of holes"
    })

    override def toList = numberOfHoles :: super.toList
  }

  /**
   *  A criterion based on the distance from agent to closest plug* and the hole closets to it.
   *  If there is more than one plug or hole to select, a random one is selected.  
   *  Is designed as an failsafe criterion, in case of 'number of holes' failure.
   *  Is designed to be used with foreseeing agent.
   */
  trait DistanceToClosestPlugAndHoleCriterion extends PlugsMovingAgentCriteria{

    /**
     *  should be negative
     */
    def distanceToClosetPlugWeight: Float
    /**
     *  should be negative
     */
    def distanceFromPlugToClosestHoleWeight: Float
    def agentId: AgentId

    def findCloset(relativelyTo: Position, cond: Tile#Snapshot => Boolean): Seq[Tile#Snapshot]
    def distance(p1: Position, p2: Position): Int

    implicit def tileSnapshotToCoordinateWrapper = (_: Tile#Snapshot).coordinate
    implicit def tileToCoordinateWrapper = (_: Tile).coordinate

    /**
     * from *all possible* hole-plug pairs
     *    choose one having minimal sum of distances from agent to plug and from plug to hole for criterion calculation,
     *    without taking in consideration agent's movement specifics, but considering which tiles the agent can walk into
     *
     */
    def distanceToClosestPlugHolePair = criterion("distance from agent to the closest plug and the plug to the closet hole",
      sn => {
        val agPos = sn.asEnv.agentsPositions(agentId)
        val closetPlugs = findCloset(agPos, _.asTile.exists(_.isPlug))
        val closetPlugHoles = closetPlugs.map(plSn => plSn -> findCloset(plSn.coordinate, _.asTile.exists(_.isHole))).toMap
        val bestPlugHoles = closetPlugHoles
          .map{case (k, v) => k -> v.filterMin(s => distance(k, s))}
          .filterMin{case (k, v) => distance(agPos, k)}
        val theBest = bestPlugHoles.collectFirst{
          case (plug, holes) if holes.nonEmpty => plug -> holes.head
        }.get
        distance(agPos, theBest._1) * distanceToClosetPlugWeight + distance(theBest._1, theBest._2) * distanceFromPlugToClosestHoleWeight
      }
    )

    override def toList = distanceToClosestPlugHolePair :: super.toList
  }
}

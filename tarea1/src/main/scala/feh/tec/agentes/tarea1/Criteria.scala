package feh.tec.agentes.tarea1

import feh.tec.util._
import feh.tec.agentes.tarea1.Agent._
import feh.tec.agent.StatelessAgentPerformanceMeasure.Criterion

object Criteria {
  trait PlugsMovingAgentCriteria extends Debugging{
    def debugMessagePrefix: String = "[Criteria] "

    protected def criterion(name: String, assess: Measure#Snapshot => Measure#Measure) =
      Criterion[Position, EnvState, EnvGlobal, Action, Env, Measure](name, assess)

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

    def numberOfHoles = criterion("number of holes", {
      s =>
        val n = s.asEnv.tiles.count(_.contents.exists(_.isHole))
        n * numberOfHolesWeight debugLog "weighted number of holes"
    })

    override def toList = numberOfHoles :: super.toList
  }

  trait DistanceToClosestPlugAndHoleCriterion extends PlugsMovingAgentCriteria{

    def distanceToClosetPlugWeight: Float
    def distanceFromPlugToClosestHoleWeight: Float

    /**
     * from *all possible* hole-plug pairs
     *    choose one having minimal sum of distances from agent to plug and from plug to hole,
     *    taking in consideration agent's movement specifics
     *
     */
    def selectClosetPlugHolePair: (Position, Position)

//    def distanceToClosestPlug = criterion("distance from agent to the closest plug")
//    def distanceFromPlugToClosestHole = criterion("distance from the closest plug to it's closet hole")
  }
}

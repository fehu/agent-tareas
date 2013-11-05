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
        val n = s.asEnv.atoms.count(_.contents.exists(_.isHole))
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

    /**
     *  todo: cannot be used to find holes
     */
    def findClosetRespectingHoles(relativelyTo: Position, cond: Tile#Snapshot => Boolean, sn: Measure.Snapshot): Seq[Tile#Snapshot]
    def findClosetDisregardingHoles(relativelyTo: Position, cond: Tile#Snapshot => Boolean, sn: Measure.Snapshot): Seq[Tile#Snapshot]

    def distanceRespectingHoles(p1: Position, p2: Position, map: Measure.Snapshot): Int
    def distanceDisregardingHoles(p1: Position, p2: Position, map: Measure.Snapshot): Int

    implicit def tileSnapshotToCoordinateWrapper = (_: Tile#Snapshot).coordinate
    implicit def tileToCoordinateWrapper = (_: Tile).coordinate

    /**
     * from *all possible* hole-plug pairs
     *    choose one having minimal sum of distances from agent to plug and from plug to hole for criterion calculation,
     *    without taking in consideration agent's movement specifics, but considering which tiles the agent can walk into
     *
     */
    def distanceToClosestPlugHolePair = criterion("distance from agent to the closest plug\n and the plug to the closet hole",
      sn => {
        val pseudoMap = sn.asEnv.worldSnapshot.asWorld
        def relativePosition = pseudoMap.relativePosition _
        val agPos = sn.asEnv.agentsPositions(agentId)
        val closetPlugs = findClosetRespectingHoles(agPos, _.asAtom.exists(_.isPlug), sn)
        val closetPlugHoles = closetPlugs.map(plSn => plSn -> findClosetDisregardingHoles(plSn.coordinate, _.asAtom.exists(_.isHole), sn)).toMap
        val bestPlugHoles = closetPlugHoles.flatMap{
          case (plugTile, holeTiles) =>
            holeTiles.flatMap{
              holeTile =>
                relativePosition(plugTile, holeTile)
                  .map(pseudoMap.tileTo(plugTile, _))
                  .filter(_.notExists(_.isHole))
                  .pipe(oppositeTiles => if(oppositeTiles.isEmpty) plugTile.asAtom :: Nil else oppositeTiles)
                  .map(_ -> holeTile)
            }
            .filterMin{
              case (p, h) => distanceDisregardingHoles(p, h, sn) + distanceRespectingHoles(agPos, p, sn)
            }
        }
        val theBest = bestPlugHoles.randomChoose
        debugLog(s"the best = $theBest")
        distanceRespectingHoles(agPos, theBest._1, sn) * distanceToClosetPlugWeight + distanceDisregardingHoles(theBest._1, theBest._2, sn) * distanceFromPlugToClosestHoleWeight
      }
    )

    override def toList = distanceToClosestPlugHolePair :: super.toList
  }
}

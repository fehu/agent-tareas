package feh.tec.tarea3

import feh.tec.agent._
import Ordered._
import feh.tec.util.SideEffect
import feh.tec.agent.StrategicChoice
import scala.math.Numeric.IntIsIntegral

class PrisonerDilemmaStrategicLayout extends DeterministicGameStrategicLayout{
  type Utility = Int
  implicit def utilityIsNumeric = IntIsIntegral

  def nPlayers = 2

  trait PrisonerPlayer extends Player{
    case object Betray extends Strategy
    case object Refuse extends Strategy

    def availableStrategies = Set(Betray, Refuse)
  }

  object A extends PrisonerPlayer
  object B extends PrisonerPlayer

  def players = Set(A, B)

  def strategicLayoutBuilder2[P1 <: Player, P2 <: Player](p1: P1, p2: P2)
                                                         (strategy1: P1 => P1#Strategy, strategy2: P2 => P2#Strategy)
                                                         (utility1: Utility, utility2: Utility): (PlayersChoices, PlayersUtility) =
    Map(p1 -> strategy1(p1), p2 -> strategy2(p2)) -> Map(p1 -> utility1, p2 -> utility2)

  def strategy = strategicLayoutBuilder2(A, B) _

  val strategicLayout: Map[PlayersChoices, PlayersUtility] = Map(
    strategy(_.Betray, _.Betray)(8, 8),
    strategy(_.Betray, _.Refuse)(0, 12),
    strategy(_.Refuse, _.Betray)(12, 0),
    strategy(_.Refuse, _.Refuse)(6, 6)
  )

  def target = Min
}

class PrisonerDilemmaGame(val strategicLayout: PrisonerDilemmaStrategicLayout)
  extends DeterministicGame[PrisonerDilemmaStrategicLayout, PrisonerDilemmaGame]
  with MutableGameImpl[PrisonerDilemmaStrategicLayout, PrisonerDilemmaGame]
{
//  type Ref = this.type

  def choose(choice: Choice): Unit = ???

  def await(): Unit = ???

  def playersScore = ???
}
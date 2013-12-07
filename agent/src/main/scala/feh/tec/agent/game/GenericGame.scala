package feh.tec.agent.game

import scala.collection.mutable
import scala.math.Numeric.IntIsIntegral

abstract class GenericGame extends AbstractGame{
  type Utility = Int
  implicit def utilityIsNumeric = IntIsIntegral

  def name: String

  trait GenericPlayer extends Player{
    def name: String
    case class NonexistentStrategyException(name: String) extends Exception(s"${this.name} player has no strategy named '$name'")
    object GenericStrategy{
      private val strategies = mutable.HashMap.empty[String, GenericStrategy]
      protected[GenericPlayer] def register(strategy: GenericStrategy){ strategies += strategy.name -> strategy }

      def getByName(name: String) = strategies.get(name)
      def byName(name: String) = getByName(name).getOrElse(throw NonexistentStrategyException(name))
    }
    case class GenericStrategy(name: String) {
      GenericStrategy.register(this)
      override def toString: String = name
    }
    type Strategy = GenericStrategy

    override def toString = s"Player#$name"
  }

}

object AbstractGenericGame{
  abstract class Game2 extends GenericGame{
    def nPlayers = 2


    protected def nameA: String
    protected def nameB: String
    protected def strategiesA: Set[A.GenericStrategy]
    protected def strategiesB: Set[B.GenericStrategy]

    object A extends GenericPlayer{
      def name = nameA
      def availableStrategies = strategiesA
    }

    object B extends GenericPlayer{
      def name = nameB
      def availableStrategies = strategiesB
    }

    def players: Set[Player] = Set(A, B)

//    protected def strategicLayoutBuilder2[P1 <: Player, P2 <: Player](p1: P1, p2: P2)
//                                                                     (strategy1: P1 => P1#Strategy, strategy2: P2 => P2#Strategy)
//                                                                     (utility1: Utility, utility2: Utility): (PlayersChoices, PlayersUtility) =
//      Map(p1 -> strategy1(p1), p2 -> strategy2(p2)) -> Map(p1 -> utility1, p2 -> utility2)

  }
}

trait GenericDeterministicGame extends GenericGame with AbstractDeterministicGame

object GenericDeterministicGame{
  case class PayoffEntry2(choice1: String,
                          choice2: String,
                          utility1: Game2#Utility,
                          utility2: Game2#Utility){
    def toTuple = (choice1, choice2) -> (utility1, utility2)
  }
  implicit def tuplesToPayoffEntry2Wrapper(p: ((String, String), (Game2#Utility, Game2#Utility))): PayoffEntry2 =
    PayoffEntry2(p._1._1, p._1._2, p._2._1, p._2._2)

  class Game2(val name: String,
              _target: Game2 => Game2#Target,
              protected val nameA: String,
              protected val nameB: String,
              bStrategiesA: Set[String],
              bStrategiesB: Set[String])
             (payoff: Either[Seq[PayoffEntry2], PartialFunction[(String, String), (Game2#Utility, Game2#Utility)]])
    extends AbstractGenericGame.Game2 with GenericDeterministicGame
  {
    def this(name: String,
             _target: Game2 => Game2#Target,
             nameA: String,
             nameB: String,
             strategies: Set[String])
            (payoffEntry: PayoffEntry2*) = this(name, _target, nameA, nameB, strategies, strategies)(Left(payoffEntry))
    def this(_target: Game2 => Game2#Target,
             name: String,
             nameA: String,
             nameB: String,
             strategies: Set[String])
            (payoff: PartialFunction[(String, String), (Game2#Utility, Game2#Utility)]) =
      this(name, _target, nameA, nameB, strategies, strategies)(Right(payoff))

    lazy val target = _target(this).asInstanceOf[Target]

    protected lazy val strategiesA = bStrategiesA.map(A.GenericStrategy.apply)
    protected lazy val strategiesB = bStrategiesB.map(B.GenericStrategy.apply)

    override lazy val layout: PartialFunction[PlayersChoices, PlayersUtility] = payoff match{
      case Right(pf) =>
        PartialFunction[PlayersChoices, (String, String)](ch => ch(A).toString -> ch(B).toString)
        .andThen(pf)
        .andThen{ case (u1, u2) => Map(A -> u1, B -> u2): PlayersUtility }
      case Left(entries) =>
        val payoffMap = entries.map(_.toTuple).map{
          case ((s1, s2), utils) => (A.GenericStrategy.byName(s1), B.GenericStrategy.byName(s2)) -> utils
        }.toMap

        Map((
          for{
            a <- strategiesA
            b <- strategiesB
            (ua, ub) = payoffMap(a, b)
          } yield ( Map(A -> a, B -> b) -> Map(A -> ua, B -> ub) ).asInstanceOf[(PlayersChoices, PlayersUtility)]
          ).toSeq: _*)
    }
  }
}

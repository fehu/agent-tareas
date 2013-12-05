package feh.tec.agent

import scala.collection.mutable

abstract class GenericGame extends AbstractGame{
  type Utility = Int
  implicit def utilityIsNumeric = implicitly[Numeric[Int]]

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
    case class GenericStrategy(name: String)
    type Strategy = GenericStrategy
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

    protected def strategicLayoutBuilder2[P1 <: Player, P2 <: Player](p1: P1, p2: P2)
                                                                     (strategy1: P1 => P1#Strategy, strategy2: P2 => P2#Strategy)
                                                                     (utility1: Utility, utility2: Utility): (PlayersChoices, PlayersUtility) =
      Map(p1 -> strategy1(p1), p2 -> strategy2(p2)) -> Map(p1 -> utility1, p2 -> utility2)

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
              bStrategies1: Set[String],
              bStrategies2: Set[String])
             (payoffEntry: PayoffEntry2*)
    extends AbstractGenericGame.Game2 with GenericDeterministicGame
  {
    lazy val target = _target(this).asInstanceOf[Target]

    protected lazy val strategiesA = bStrategies1.map(A.GenericStrategy.apply)
    protected lazy val strategiesB = bStrategies2.map(B.GenericStrategy.apply)

    
    protected lazy val payoffMap = payoffEntry.map(_.toTuple).map{
      case ((s1, s2), utils) => (A.GenericStrategy.byName(s1), B.GenericStrategy.byName(s2)) -> utils
    }.toMap

    override lazy val layout: Map[PlayersChoices, PlayersUtility] = Map((
      for{
        a <- strategiesA
        b <- strategiesB
        (ua, ub) = payoffMap(a, b)
      } yield ( Map(A -> a, B -> b) -> Map(A -> ua, B -> ub) ).asInstanceOf[(PlayersChoices, PlayersUtility)]
    ).toSeq: _*)
  }
}

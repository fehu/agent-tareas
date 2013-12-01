package feh.tec.agent

import scala.collection.mutable
import feh.tec.util._

object AgentDecision{
  trait AbstractDecision[D]{
    def decision: D
  }

  trait DecisionStrategy[Action <: AbstractAction, -Arg, +Decision]{
    def name: String
    def decide: Arg => Decision
    
    def reset()
  }


  trait FailsafeDecisionStrategy[Action <: AbstractAction, Arg, Decision] extends DecisionStrategy[Action, Arg, Decision] with Debugging{
    def decisionsChain: Seq[FailsafeDecisionStrategy.FailProbeStrategy[Action, Arg, Decision]]
    def lastStrategy: DecisionStrategy[Action, Arg, Decision]

    
    
    def decide: (Arg) => Decision = arg => (Option.empty[Decision] /: decisionsChain)(
      (acc, str) =>
        acc.$(_ => str.strategy.reset()) orElse {
          str.strategy.decide(arg) match{
            case dec if str fail dec =>
              debugLog(s"strategy '${str.strategy.name}' has failed, proceeding to failsafe strategy")
              None
            case dec =>
              debugLog(s"strategy '${str.strategy.name}' provided an acceptable decision to proceed")
              Some(dec)
          }
        }
      ) $ (_ => lastStrategy.reset()) getOrElse {
        debugLog(s"using last strategy ${lastStrategy.name}")
        lastStrategy.decide(arg)
      }
  }
  
  object FailsafeDecisionStrategy{
    var Debug = false

    case class FailProbeStrategy[Action <: AbstractAction, Arg, Decision](fail: Decision => Boolean, strategy: DecisionStrategy[Action, Arg, Decision])

    def Builder[Action <: AbstractAction, Arg, Decision](init: DecisionStrategy[Action, Arg, Decision]): Builder[Action, Arg, Decision] = new Builder[Action, Arg, Decision](init)

    class Builder[Action <: AbstractAction, Arg, Decision](init: DecisionStrategy[Action, Arg, Decision]){
      builder =>

      protected val decisionsChain = mutable.Buffer[FailProbeStrategy[Action, Arg, Decision]]()
      protected var lastStrategy = init
      
      def append(previousFailed: Decision => Boolean, strategy: DecisionStrategy[Action, Arg, Decision]): Builder[Action, Arg, Decision] = {
        decisionsChain += FailProbeStrategy(previousFailed, lastStrategy)
        lastStrategy = strategy
        this
      }

      def build(): FailsafeDecisionStrategy[Action, Arg, Decision]  = new FailsafeDecisionStrategy[Action, Arg, Decision]{
        val decisionsChain: Seq[FailProbeStrategy[Action, Arg, Decision]] = builder.decisionsChain
        val lastStrategy: DecisionStrategy[Action, Arg, Decision] = builder.lastStrategy

        val debug: Boolean = Debug
        val debugMessagePrefix: String = "[FailsafeDecisionStrategy] "
        val name: String = "chained strategy: " + (decisionsChain.map(_.strategy.name) :+ lastStrategy.name).mkString(",")

        def reset(): Unit = {
          decisionsChain.foreach(_.strategy.reset())
          lastStrategy.reset()
        }
      }
    }
  }
  
  case class ExtendedCriteriaBasedDecision[D, Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                           Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                           Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                           M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  (decision: D, consideredOptionsCriteriaValues: Seq[Set[M#CriterionValue]]) extends AbstractDecision[D]
  {
    def map[R](f: D => R): ExtendedCriteriaBasedDecision[R, Position, EnvState, EnvGlobal, Action, Env, Exec, M] =
      copy(f(decision), consideredOptionsCriteriaValues)

    def flatMap[R](f: D => Seq[R]): Seq[ExtendedCriteriaBasedDecision[R, Position, EnvState, EnvGlobal, Action, Env, Exec, M]] =
      f(decision).map(ExtendedCriteriaBasedDecision[R, Position, EnvState, EnvGlobal, Action, Env, Exec, M](_, consideredOptionsCriteriaValues))
  }


  trait ExplainedAction[Action]{
    type Explanation

    def action: Action
    def explain: Explanation
  }

  case class ExplainedActionStub[Action](action: Action) extends ExplainedAction[Action]{
    type Explanation = None.type
    def explain: Explanation = None
  }

  trait CriteriaReasonedExplanation[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                    Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                    Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                    M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  {
    type Explanation = (Set[M#CriterionValue], M#Measure)

    def criteria: Set[M#CriterionValue]
    def measure: M#Measure
    def message: CriteriaMessage[M#CriterionValue]

    def explain: Explanation = criteria -> measure
  }

  case class CriteriaMessage[CriterionValue](beforeCriteria: String = null, afterCriteria: String = null){
    def beforeOpt = Option(beforeCriteria)
    def afterOpt = Option(afterCriteria)
  }

  case class CriteriaReasonedDecisions[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                       Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                       Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                       M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
    (action: Seq[Action], criteria: Set[M#CriterionValue], measure: M#Measure, message: CriteriaMessage[M#CriterionValue] = CriteriaMessage[M#CriterionValue]())
      extends ExplainedAction[Seq[Action]] with CriteriaReasonedExplanation[Position, EnvState, EnvGlobal, Action, Env, Exec, M]
  {
    def toSeq = action.map(d => CriteriaReasonedDecision[Position, EnvState, EnvGlobal, Action, Env, Exec, M](d, criteria, measure, message))
  }


  case class CriteriaReasonedDecision[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                      Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                      Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                      M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
    (action: Action, criteria: Set[M#CriterionValue], measure: M#Measure, message: CriteriaMessage[M#CriterionValue] = CriteriaMessage[M#CriterionValue]())
      extends ExplainedAction[Action] with CriteriaReasonedExplanation[Position, EnvState, EnvGlobal, Action, Env, Exec, M]
}

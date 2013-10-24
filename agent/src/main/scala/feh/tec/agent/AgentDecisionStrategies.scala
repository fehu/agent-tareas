package feh.tec.agent

import feh.tec.agent.AgentDecision._
import feh.tec.agent.AgentDecision.CriteriaReasonedDecision
import feh.tec.util.{RandomWrappers, FilteringHelpingWrapper}
import RandomWrappers._

object IdealRationalAgentDecisionStrategies {
  class MeasureBasedDecisionStrategy[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                     Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with PredictableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                                     Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                     M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M],
                                     Ag <: IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M] with AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]]
    extends DecisionStrategy[Action, Ag#DecisionArg, Ag#ActionExplanation]
  {
    case class CriteriaExtendedDecision(decision: Ag#ActionExplanation, allPredictionsCriteriaValue: Seq[M#Measure]) extends ExtendedDecision[Ag#ActionExplanation]

    type DExtended = CriteriaExtendedDecision

    def decide: Ag#DecisionArg => CriteriaExtendedDecision = _ match{
      case (ag, possibleActions) =>
        implicit val num = ag.measure.measureNumeric.asInstanceOf[Numeric[M#Measure]]
        val possibleDecisions = for{
          a <- possibleActions
          prediction = ag.env.predict(a)
          criteriaV = ag.calcPerformance(prediction)
          performance = criteriaV.map(_.value).sum
        } yield CriteriaReasonedDecision[Position, EnvState, EnvGlobal, Action, Env, Exec, M](a, criteriaV.toSet, performance)

        CriteriaExtendedDecision(possibleDecisions.filterMax(_.measure).randomChoose, possibleDecisions.toSeq.map(_.measure))
    }

    def failsafe(sFail: (DExtended) => Boolean,
                 onFail: (Ag#DecisionArg, DExtended) => DExtended): FailsafeDecisionStrategy[Action, Ag#DecisionArg, Ag#ActionExplanation, this.type] =
      GenericFailsafeDecisionStrategy[Action, Ag#DecisionArg, Ag#ActionExplanation, this.type](this, sFail, onFail)
  }
}

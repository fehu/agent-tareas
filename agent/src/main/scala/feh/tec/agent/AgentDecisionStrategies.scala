package feh.tec.agent

import feh.tec.agent.AgentDecision._
import feh.tec.agent.AgentDecision.CriteriaReasonedDecision
import feh.tec.util.{RandomWrappers, FilteringHelpingWrapper}
import RandomWrappers._
import feh.tec.agent.AgentDecisionStrategies.FailsafeImpl
import scala.collection.mutable

object AgentDecisionStrategies{
  trait FailsafeImpl[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                     Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with PredictableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                     Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                     M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M],
                     Ag <: IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M] with AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]]{
    self: DecisionStrategy[Action, Ag#DecisionArg, Ag#ActionExplanation] =>

    def failsafe(sFail: (DExtended) => Boolean,
                 onFail: (Ag#DecisionArg, DExtended) => DExtended): FailsafeDecisionStrategy[Action, Ag#DecisionArg, Ag#ActionExplanation, this.type] =
      GenericFailsafeDecisionStrategy[Action, Ag#DecisionArg, Ag#ActionExplanation, this.type](this, sFail, onFail)
  }
}

object IdealRationalAgentDecisionStrategies {
  class MeasureBasedDecisionStrategy[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                     Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with PredictableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                                     Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                     M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M],
                                     Ag <: IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M] with AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]]
    extends DecisionStrategy[Action, Ag#DecisionArg, Ag#ActionExplanation] with FailsafeImpl[Position, EnvState, EnvGlobal, Action, Env, Exec, M, Ag]
  {
    case class CriteriaExtendedDecision(decision: Ag#ActionExplanation, allPredictionsCriteriaValue: Seq[M#Measure]) extends ExtendedDecision[Ag#ActionExplanation]

    type DExtended = CriteriaExtendedDecision

    def failsafe(sFail: (DExtended) => Boolean, onFail: M#Criteria): FailsafeDecisionStrategy[Action, Ag#DecisionArg, Ag#ActionExplanation, this.type] =
      failsafe(sFail, (arg, _) => arg match{
        case (ag, possibleActions) => ag.withCriteria(onFail)(decide(arg))
      })

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

  }
}

object IdealForeseeingAgentDecisionStrategies{

  class MeasureBasedForeseeingDecisionStrategy[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                               Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with ForeseeableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                                               Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                               M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M],
                                               Ag <: IdealForeseeingDummyAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M] with AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]]
      (val foreseeingDepth: Int)
    extends IdealRationalAgentDecisionStrategies.MeasureBasedDecisionStrategy[Position, EnvState, EnvGlobal, Action, Env, Exec, M, Ag]
  {
    type ReasonedDecisions = CriteriaReasonedDecisions[Position, EnvState, EnvGlobal, Action, Env, Exec, M]

    case class CriteriaExtendedDecisions(decisions: ReasonedDecisions, allPredictionsCriteriaValue: Seq[M#Measure]) extends ExtendedDecision[Seq[Ag#ActionExplanation]]{
      def decision: Seq[Ag#ActionExplanation] = decisions.toSeq
      def extendedDecisions: Seq[CriteriaExtendedDecision] = decision.map(d => CriteriaExtendedDecision(d, allPredictionsCriteriaValue))
    }

    protected var currentDecisions: Option[CriteriaExtendedDecisions] = None
    protected val executeQueue = mutable.Queue.empty[CriteriaExtendedDecision]

    protected def makeDecisions(arg: Ag#DecisionArg): CriteriaExtendedDecisions = arg match {
      case (ag, possibleActions) =>
        implicit val num = ag.measure.measureNumeric.asInstanceOf[Numeric[M#Measure]]
        def behavioursFunc = ag.perceiveFromSnapshot _ andThen ag.possibleBehaviors
        val tacticalOptions = ag.env.foresee(foreseeingDepth, behavioursFunc)
        val estimatedPerformance =  tacticalOptions.map{ case (actions, result) => actions -> ag.calcPerformance(result) }
        val estimatedMeasures = estimatedPerformance.mapValues(v => v -> v.map(_.value).sum)
        val bestOptions =  estimatedMeasures.filterMax(_._2._2)
        val chosen = bestOptions.randomChoose
        val criteriaMsg = CriteriaMessage[M#CriterionValue](
          beforeCriteria = "The action is part of decision sequence made by actor,\n the criteria values are presented for the whole sequence"
        )
        CriteriaExtendedDecisions(new ReasonedDecisions(chosen._1, chosen._2._1.toSet, chosen._2._2, criteriaMsg), estimatedMeasures.map(_._2._2).toSeq)
    }

    override def decide: Ag#DecisionArg => CriteriaExtendedDecision = { arg =>
      if(executeQueue.isEmpty) {
        currentDecisions = Option(makeDecisions(arg))
        executeQueue.enqueue(currentDecisions.get.extendedDecisions: _*)
      }
      executeQueue.dequeue()
    }
  }
}
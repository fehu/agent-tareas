package feh.tec.agent

import feh.tec.agent.AgentDecision._
import feh.tec.agent.AgentDecision.CriteriaReasonedDecision
import feh.tec.util.{GlobalDebugging, RandomWrappers, FilteringHelpingWrapper}
import RandomWrappers._
import scala.collection.mutable
import java.util.Calendar
import feh.tec.util._

object IdealRationalAgentDecisionStrategies {
  class MeasureBasedDecisionStrategy[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                     Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with PredictableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                                     Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                     M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M],
                                     Ag <: IdealRationalAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M] with AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]]
    extends DecisionStrategy[Action, Ag#DecisionArg, ExtendedCriteriaBasedDecision[Ag#ActionExplanation, Position, EnvState, EnvGlobal, Action, Env, Exec, M]]
  {
    outer =>

    def name: String = "Measure Based Decision Strategy"

    def decide: (Ag#DecisionArg) => ExtendedCriteriaBasedDecision[Ag#ActionExplanation, Position, EnvState, EnvGlobal, Action, Env, Exec, M] =
      {
        case (ag, possibleActions) =>
          implicit val num = ag.measure.measureNumeric.asInstanceOf[Numeric[M#Measure]]
          val possibleDecisions = for {
            a <- possibleActions
            prediction = ag.env.predict(a)
            criteriaV = rewriteCriteria.map(ag.withCriteria(_)(ag.calcPerformance(prediction))) getOrElse ag.calcPerformance(prediction)
            performance = criteriaV.map(_.value).sum
          } yield CriteriaReasonedDecision[Position, EnvState, EnvGlobal, Action, Env, Exec, M](a, criteriaV.toSet, performance)

          ExtendedCriteriaBasedDecision(possibleDecisions.filterMax(_.measure).randomChoose, possibleDecisions.toSeq.map(_.criteria))
      }

    lazy val rewriteCriteria: Option[M#Criteria] = None

  }
}

object IdealForeseeingAgentDecisionStrategies{

  class MeasureBasedForeseeingDecisionStrategy[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                               Env <: Environment[Position, EnvState, EnvGlobal, Action, Env] with ForeseeableEnvironment[Position, EnvState, EnvGlobal, Action, Env],
                                               Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                               M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M],
                                               Ag <: IdealForeseeingDummyAgent[Position, EnvState, EnvGlobal, Action, Env, Exec, M] with AgentExecution[Position, EnvState, EnvGlobal, Action, Env, Exec]]
      (val foreseeingDepth: Int, val debug: Boolean)
    extends IdealRationalAgentDecisionStrategies.MeasureBasedDecisionStrategy[Position, EnvState, EnvGlobal, Action, Env, Exec, M, Ag] with Debugging
  {
    override def name: String = "Foreseeing Measure Based Decision Strategy"

    def debugMessagePrefix: String = "[IdealForeseeingAgentDecisionStrategies: MeasureBasedForeseeingDecisionStrategy] "

    type ReasonedDecisions = CriteriaReasonedDecisions[Position, EnvState, EnvGlobal, Action, Env, Exec, M]

    type Decisions = ExtendedCriteriaBasedDecision[ReasonedDecisions, Position, EnvState, EnvGlobal, Action, Env, Exec, M]
    type Decision = ExtendedCriteriaBasedDecision[Ag#ActionExplanation, Position, EnvState, EnvGlobal, Action, Env, Exec, M]

    protected var currentDecisions: Option[Decisions] = None
    protected val executeQueue = mutable.Queue.empty[Decision]

    protected def makeDecisions(arg: Ag#DecisionArg): Decisions = arg match {
      case (ag, possibleActions) =>
        implicit val num = ag.measure.measureNumeric.asInstanceOf[Numeric[M#Measure]]
        def behavioursFunc = ag.perceiveFromSnapshot _ andThen ag.possibleBehaviors
        val tacticalOptions = ag.env.foresee(foreseeingDepth, behavioursFunc)
        def calcPerformance(prediction: Env#Prediction) = {
          val (r, t) = elapsed(rewriteCriteria.map(ag.withCriteria(_)(ag.calcPerformance(prediction))) getOrElse ag.calcPerformance(prediction))
          debugLog(s"performance calc time: $t}")
          r
        }
        val estimatedPerformance = tacticalOptions.map{ case (actions, result) => actions -> calcPerformance(result)}
        val estimatedMeasures = estimatedPerformance.mapValues(v => v -> v.map(_.value).sum)
        val bestOptions = estimatedMeasures.filterMax(_._2._2)
        val chosen = bestOptions.randomChoose
        val criteriaMsg = CriteriaMessage[M#CriterionValue](
          beforeCriteria = "The action is part of decision sequence made by actor,\n the criteria values are presented for the whole sequence"
        )
        ExtendedCriteriaBasedDecision(new ReasonedDecisions(chosen._1, chosen._2._1.toSet, chosen._2._2, criteriaMsg), estimatedMeasures.map(_._2._1.toSet).toSeq)
    }

    override def decide: (Ag#DecisionArg) => Decision = {
      arg =>
        if(executeQueue.isEmpty) {
          currentDecisions = Option(makeDecisions(arg))
          executeQueue.enqueue(currentDecisions.get.flatMap(_.toSeq): _*)
        }
        executeQueue.dequeue()
    }
  }
}
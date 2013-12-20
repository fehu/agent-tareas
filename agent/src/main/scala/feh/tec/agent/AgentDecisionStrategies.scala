package feh.tec.agent

import feh.tec.agent.AgentDecision._
import feh.tec.agent.AgentDecision.CriteriaReasonedDecision
import scala.collection.mutable
import feh.tec.util._

object IdealRationalAgentDecisionStrategies {
  class MeasureBasedDecisionStrategy[Env <: Environment[Env] with PredictableEnvironment[Env],
                                     M <: AgentPerformanceMeasure[Env, M],
                                     Ag <: IdealRationalAgent[Env, Exec, M] with AgentExecution[Env, Exec] forSome {type Exec <: AgentExecutionLoop}]
  (agentPosition: Env#Prediction => Env#Coordinate)
    extends DecisionStrategy[Env#Action, Ag#DecisionArg, ExtendedCriteriaBasedDecision[Ag#ActionExplanation, Env, M]]
  {
    outer =>

    def name: String = "Measure Based Decision Strategy"

    def decide: (Ag#DecisionArg) => ExtendedCriteriaBasedDecision[Ag#ActionExplanation, Env, M] =
      {
        case (ag, possibleActions) =>
          implicit val num = ag.measure.measureIsNumeric.asInstanceOf[Numeric[M#Measure]]
          val possibleDecisions = for {
            a <- possibleActions
            prediction = ag.env.predict(a)
            criteriaV = rewriteCriteria.map(ag.withCriteria(_)(ag.calcPerformance(prediction))) getOrElse ag.calcPerformance(prediction)
            performance = criteriaV.map(_.value).sum
          } yield CriteriaReasonedDecision[Env, M](a, criteriaV.toSet, performance)

          ExtendedCriteriaBasedDecision(possibleDecisions.filterMax(_.measure).randomChoose, possibleDecisions.toSeq.map(_.criteria))
      }

    lazy val rewriteCriteria: Option[M#Criteria] = None

    def reset(): Unit = {}
  }
}

object IdealForeseeingAgentDecisionStrategies{

  class MeasureBasedForeseeingDecisionStrategy[Env <: Environment[Env] with ForeseeableEnvironment[Env] ,
                                               M <: AgentPerformanceMeasure[Env, M],
                                               Ag <: IdealForeseeingSimpleAgent[Env, Exec, M] with AgentExecution[Env, Exec] forSome {type Exec <: AgentExecutionLoop} ]
      (val foreseeingDepth: Int, val debug: Boolean, notifyRouteChosen: Option[Seq[Env#Action]] => Unit)
    extends IdealRationalAgentDecisionStrategies.MeasureBasedDecisionStrategy[Env, M, Ag](null) with Debugging
  {
    override def name: String = "Foreseeing Measure Based Decision Strategy"

    def debugMessagePrefix: String = "[IdealForeseeingAgentDecisionStrategies: MeasureBasedForeseeingDecisionStrategy] "

    type ReasonedDecisions = CriteriaReasonedDecisions[Env, M]

    type Decisions = ExtendedCriteriaBasedDecision[ReasonedDecisions, Env, M]
    type Decision = ExtendedCriteriaBasedDecision[Ag#ActionExplanation, Env, M]

    private var _currentDecisions: Option[Decisions] = None
    def currentDecisions = _currentDecisions
    protected def currentDecisions_=(opt: Option[Decisions])(implicit ag: Ag) {
      _currentDecisions = opt
      notifyRouteChosen(opt.map(_.decision.toSeq.map(_.action)))
    } 
    protected val executeQueue = mutable.Queue.empty[Decision]

    def tacticalOptionsIncludeShorter = true
    def tacticalOptionsExcludeTurningBack = true
    def tacticalOptionsFilterAgentIgnoringComparator: Option[(Env#State, Env#State) => Boolean] = None
    final def tacticalOptionsFilterLeadingToSameState = tacticalOptionsFilterAgentIgnoringComparator.isDefined
    def bestOptionsPreferShorter = true
//    def filter


    protected def makeDecisions(arg: Ag#DecisionArg): Decisions = arg match {
      case (ag, possibleActions) =>
        implicit val num = ag.measure.measureIsNumeric
        def behavioursFunc = ag.perceiveFromSnapshot _ andThen ag.possibleBehaviors
        def filterTacticalOptions(ops: Map[Seq[Env#Action], Env#Prediction]) =
          tacticalOptionsFilterAgentIgnoringComparator.map{
            compar =>
              EnvironmentSnapshot.withStateComparator(compar){
                ops.groupBy(_._2).map{
                  case (prediction, map) => map.keys.toSeq.randomChoice -> prediction
                }
              }
          } getOrElse ops



        val tacticalOptions = ag.env
          .foresee(foreseeingDepth, _ => behavioursFunc, includeShorter = tacticalOptionsIncludeShorter, excludeTurningBack = tacticalOptionsExcludeTurningBack)
          .asInstanceOf[Map[Seq[Env#Action], Env#Prediction]]
          .pipe(filterTacticalOptions)
          .debugLog(ops => s"considering ${ops.size} tactical options")
          .debugLogElapsedTime("tacticalOptions calc time: "+)
        def calcPerformance(prediction: Env#Prediction) =
          (rewriteCriteria.map(ag.withCriteria(_)(ag.calcPerformance(prediction))) getOrElse ag.calcPerformance(prediction))
            .debugLogElapsedTime("performance calc time: "+)
        val estimatedPerformance = tacticalOptions.map{ case (actions, result) => actions -> calcPerformance(result)}
          .debugLogElapsedTime("estimatedPerformance calc time: "+)
        val estimatedMeasures = estimatedPerformance.mapValues(v => v -> v.map(_.value).sum)
        val bestOptions = estimatedMeasures.filterMax(_._2._2)
        val chosen = (
            if(tacticalOptionsIncludeShorter && bestOptionsPreferShorter) bestOptions.filterMin(_._1.length)
            else bestOptions
          ).randomChoose
        val criteriaMsg = CriteriaMessage[M#CriterionValue](
          beforeCriteria = "The action is part of decision sequence made by actor,\n the criteria values are presented for the whole sequence"
        )
        ExtendedCriteriaBasedDecision(new ReasonedDecisions(chosen._1, chosen._2._1.toSet, chosen._2._2, criteriaMsg), estimatedMeasures.map(_._2._1.toSet).toSeq)
    }

    override def decide: (Ag#DecisionArg) => Decision = {
      arg =>
        if(executeQueue.isEmpty) {
          implicit def ag = arg._1
          currentDecisions = Option(makeDecisions(arg))
          executeQueue.enqueue(currentDecisions.get.flatMap(_.toSeq): _*)
        }
        executeQueue.dequeue()
    }

    override def reset(): Unit = executeQueue.clear()
  }
}
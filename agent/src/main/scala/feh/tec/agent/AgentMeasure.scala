package feh.tec.agent

import scala.collection.TraversableLike
import feh.tec.agent.StatelessAgentPerformanceMeasure.CalculatedCriterion

trait AgentMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                   M <: AgentMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
{
  type Arguments
  type Measure
  implicit val measureNumeric: Numeric[Measure]

  def measure(arg: Arguments): Measure

  type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]
  type Ag = IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env]
  type Snapshot = EnvironmentSnapshot[Position, EnvState, EnvGlobal, Action, Env]
}

trait AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                              Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                              M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  extends AgentMeasure[Position, EnvState, EnvGlobal, Action, Env, M]
{
  type CriterionArgs

  type Criterion <: AgentPerformanceMeasureCriterion[Position, EnvState, EnvGlobal, Action, Env, M]
  type Criteria = Seq[Criterion]

  type CriterionValue <: AgentPerformanceMeasureCriterionValue[Position, EnvState, EnvGlobal, Action, Env, M]
  type CriteriaValue = Seq[CriterionValue]
}

trait AgentPerformanceMeasureCriterion[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                       Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                       M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]{
  def name: String
  def assess:  M#CriterionArgs => M#Measure
}

trait AgentPerformanceMeasureCriterionValue[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                            Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                            M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]{
  def name: String
  def value:  M#Measure
}

trait StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                       Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                       M <: StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  extends AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]
{
  self: M =>


  type CriterionArgs = Snapshot
  type Criterion = StatelessAgentPerformanceMeasure.Criterion[Position, EnvState, EnvGlobal, Action, Env, M]
  type CriterionValue = StatelessAgentPerformanceMeasure.CalculatedCriterion[Position, EnvState, EnvGlobal, Action, Env, M]
  type Arguments = (CriterionArgs, Criteria)


  def performanceDebug = false

  def performance(snapshot: Snapshot)(implicit criteria: Criteria): CriteriaValue =
    (Seq.empty[CriterionValue] /: criteria){ (acc, criterion) =>
      val m = criterion.calculate(snapshot)
      if(performanceDebug) println(s"[Measure Performance] ${criterion.name}: $m}")
      acc :+ m
    }

  def measure(arg: Arguments): Measure = performance(arg._1)(arg._2).map(_.value).sum(measureNumeric.asInstanceOf[Numeric[M#Measure]]).asInstanceOf[Measure]
}

class StatelessAgentPerformanceDoubleMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                             Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                             M <: StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  extends StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]
{
  self: M =>

  type Measure = Double
  implicit val measureNumeric = Numeric.DoubleIsFractional
}


object StatelessAgentPerformanceMeasure{
  case class Criterion[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                       Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                       M <: StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
    (name: String, assess: M#Snapshot => M#Measure) extends AgentPerformanceMeasureCriterion[Position, EnvState, EnvGlobal, Action, Env, M]
  {
    def calculate(sn: M#Snapshot): CalculatedCriterion[Position, EnvState, EnvGlobal, Action, Env, M] =
      CalculatedCriterion(name, assess(sn))
  }
  case class CalculatedCriterion[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                 Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                 M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
    (name: String, value: M#Measure) extends AgentPerformanceMeasureCriterionValue[Position, EnvState, EnvGlobal, Action, Env, M]
}
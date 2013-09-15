package feh.tec.agent

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
                              M <: AgentMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  extends AgentMeasure[Position, EnvState, EnvGlobal, Action, Env, M]


trait StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                       Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                       M <: StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  extends AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]
{
  self: M =>

  import StatelessAgentPerformanceMeasure._

  type Arguments = (Snapshot, Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, M]])

  import measureNumeric._

  def performance(snapshot: Snapshot)(implicit criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, M]]): Measure =
    (zero /: criteria)( (acc, criterion) =>
      acc + criterion.assess(snapshot).asInstanceOf[Measure]
  )

  def measure(arg: Arguments): Measure = performance(arg._1)(arg._2)
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
    (assess: M#Snapshot => M#Measure)
}
package feh.tec.agent

trait AgentMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                   Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
{
  type Arguments
  type Measure
  implicit val measureNumeric: Numeric[Measure]

  def measure(arg: Arguments): Measure

  type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]
  type Ag = Agent[Position, EnvState, EnvGlobal, Action, Env, Exec]
  type Snapshot = EnvironmentSnapshot[Position, EnvState, EnvGlobal, Action, Env]
}

trait AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                       Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                       Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends AgentMeasure[Position, EnvState, EnvGlobal, Action, Env, Exec]


trait StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                              Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                              Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  import StatelessAgentPerformanceMeasure._

  type Arguments = (Snapshot, Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Exec, this.type]])

  import measureNumeric._

  def performance(snapshot: Snapshot)(implicit criteria: Seq[Criterion[Position, EnvState, EnvGlobal, Action, Env, Exec, this.type]]): Measure =
    (zero /: criteria)( (acc, criterion) =>
      acc + criterion.assess(snapshot)
  )

  def measure(arg: Arguments): Measure = performance(arg._1)(arg._2)
}

object StatelessAgentPerformanceMeasure{
  case class Criterion[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                       Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                       Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                       M <: StatelessAgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, Exec]]
    (assess: M#Snapshot => M#Measure)
}
package feh.tec.agent

trait AgentMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                   Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
{
  type MeasureUnit
  def measureNumeric: Numeric[MeasureUnit]

  type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]
  type Ag = Agent[Position, EnvState, EnvGlobal, Action, Env, Exec]

  def measuring: EnvRef => Ag => MeasureUnit
}

trait AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                              Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                              Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends AgentMeasure[Position, EnvState, EnvGlobal, Action, Env, Exec]
{

}
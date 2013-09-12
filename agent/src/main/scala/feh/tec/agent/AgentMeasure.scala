package feh.tec.agent

trait AgentMeasure[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                   Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
{
  type MeasureUnit <: Comparable[MeasureUnit]

  type EnvRef = EnvironmentRef[Position, EnvState, EnvGlobal, Action, Env]
  type Ag = Agent[Position, EnvState, EnvGlobal, Action, Env, Exec]
}

trait AgentPerformancePredictor[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                              Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                              Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env]]
  extends AgentMeasure[Position, EnvState, EnvGlobal, Action, Env, Exec]
{
  def predictPerformance
}
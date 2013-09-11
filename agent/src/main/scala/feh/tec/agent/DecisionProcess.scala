package feh.tec.agent

sealed trait DecisionProcess[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                             Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env], Ag <: Agent[Position, EnvState, EnvGlobal, Action, Env, Exec]]
{
  type Perceived
  type Decision
}


/*

  have:
    N clusters, having 2M_i (hole, plug) pairs,
      VC_i - inner i-th cluster variance,
      VP_i - mean variance
    cluster's contour must intersect none other clusters' contours

*/
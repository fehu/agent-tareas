package feh.tec.visual.api

import feh.tec.agent.{Environment, AbstractAction, AgentPerformanceMeasure}

trait CriterionValueRenderer[Coord, State, Global, Action <: AbstractAction,
                            Env <: Environment[Coord, State, Global, Action, Env],
                            M <: AgentPerformanceMeasure[Coord, State, Global, Action, Env, M],
                            E <: Easel] extends Renderer[M#CriterionValue, E]{
  def render(t: M#CriterionValue)(implicit easel: E): Unit
}
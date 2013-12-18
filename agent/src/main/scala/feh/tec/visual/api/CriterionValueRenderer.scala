package feh.tec.visual.api

import feh.tec.agent.{Environment, AgentPerformanceMeasure}

trait CriterionValueRenderer[Env <: Environment[Env], M <: AgentPerformanceMeasure[Env, M], E <: Easel]
  extends Renderer[M#CriterionValue, E]
{
  def render(t: M#CriterionValue)(implicit easel: E): Unit
}
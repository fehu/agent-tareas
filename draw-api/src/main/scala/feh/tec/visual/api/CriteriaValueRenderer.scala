package feh.tec.visual.api

import feh.tec.agent.{Environment, AbstractAction, AgentPerformanceMeasure}
import akka.actor.ActorIdentity
import java.awt.Color

trait CriteriaValueRenderer[Coord, State, Global, Action <: AbstractAction,
                             Env <: Environment[Coord, State, Global, Action, Env],
                             M <: AgentPerformanceMeasure[Coord, State, Global, Action, Env, M],
                             E <: Easel] extends Renderer[M#CriteriaValue, E]{
  def render(t: M#CriteriaValue)(implicit easel: E): Unit = t foreach render
  def render(t: M#CriterionValue)(implicit easel: E): Unit
}
package feh.tec.agent

/**
 * provides access to current environment instance, which is hidden from agent
 */
trait EnvironmentRef[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action]]{
  protected def overseer: EnvironmentOverseer[Coordinate, State, Global, Action, Env]

  def globalState: Global
  def stateOf(c: Coordinate): Option[State]
  def affected(act: Action): Environment[Coordinate, State, Global, Action]
  def visibleStates: Map[Coordinate, State]

  /**
   * makes a snapshot (a static image) of current environment state
   */
  def snapshot: Env with EnvironmentSnapshot[Coordinate, State, Global, Action, Env]
}

/**
 * should be mixed-in last
 */
trait EnvironmentSnapshot[Coordinate, State, Global, Action <: AbstractAction, Env <: Environment[Coordinate, State, Global, Action]]
  extends Environment[Coordinate, State, Global, Action] /*with Determinism[Coordinate, State, Global, Action]*/{
  self: Env =>

  /**
   * @return self, no effect should be produced
   */
//  abstract override def affected(act: Action): EnvironmentSnapshot[Coordinate, State, Global, Action, Env] = this
  def affected(act: Action): self.type = this
}

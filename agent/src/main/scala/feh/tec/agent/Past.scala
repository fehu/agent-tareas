package feh.tec.agent

trait Past[Coordinate, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Coordinate, EnvState, EnvGlobal, Action, Env],
           Ag <: Agent[Coordinate, EnvState, EnvGlobal, Action, Env]] {

  type History = (EnvironmentSnapshot[Coordinate, EnvState, EnvGlobal, Action, Env], Action)

  def history: Seq[History]
}

package feh.tec.agent

trait Past[Env <: Environment[Env]] {

  type History = (EnvironmentSnapshot[Env], Env#Action)

  def history: Seq[History]
}

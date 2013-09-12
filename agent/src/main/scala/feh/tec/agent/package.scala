package feh.tec

package object agent {
  type AbstractAgent[Position, EnvState, EnvGlobal, Action <: AbstractAction, Env <: Environment[Position, EnvState, EnvGlobal, Action, Env]] =
    IndecisiveAgent[Position, EnvState, EnvGlobal, Action, Env]
}

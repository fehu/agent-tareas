package feh.tec

package object agent {
  type AbstractAgent[Env <: Environment[Env]] = IndecisiveAgent[Env]
}

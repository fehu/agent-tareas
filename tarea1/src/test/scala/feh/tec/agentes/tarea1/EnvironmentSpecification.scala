package feh.tec.agentes.tarea1

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.{Gen, Arbitrary}
import feh.tec.agentes.tarea1.Tarea1.{Environment => Env}

class EnvironmentSpecification extends Specification with ScalaCheck{
  import Arbitraries._

  "The Environment" should{
    "be accessible at all coordinates defined" in {
      prop{ env: Environment => env.definedAt forall env.get.isDefinedAt }
    }
  }

  object Arbitraries{

    implicit def arbitraryFullyAccessibleEnvironment: Arbitrary[Environment] = Arbitrary{
      for { x <- Gen.chooseNum(10, 100); y <- Gen.chooseNum(10, 100) }
      yield new Environment(
        DummyMapGenerator.buildTilesMap(1 to x, 1 to y)((_, _) => None) andThen (_.values.toSeq),
        1 to x,
        1 to y,
        Env.effects,
        Env.initGlobal,
        Env.mapStateBuilder
      )
    }

  }
}

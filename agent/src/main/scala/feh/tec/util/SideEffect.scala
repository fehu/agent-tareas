package feh.tec.util

class SideEffect[+R](effect: => R){
  lazy val result = effect

  def execute = result

  def more[R2](other: => R2) = new SideEffect[R2]({effect; other})
  def then[R2](f: R => R2): SideEffect[R2] = more(f(effect))
  def flatThen[R2](f: R => SideEffect[R2]): SideEffect[R2] = more(f(effect).execute)

  def foreach(eff: R => Unit) = new SideEffect[R]({val e = effect; eff; e})
}

object SideEffect{
  def apply[R](effect: => R): SideEffect[R] = new SideEffect(effect)

  def sideEffect[R](effect: => R): SideEffect[R] = apply(effect)

  implicit def unitToSideEffect = (_: Unit) => sideEffect{}

  implicit class ToSideEffectWrapper[R](r: => R){
    def asSideEffect = SideEffect(r)
    def before[R2](eff: SideEffect[R2]): SideEffect[R] = sideEffect{eff.execute; r}
    def then[R2](eff: SideEffect[R2]): SideEffect[R2] = sideEffect{r; eff.execute}
  }

  implicit class SideEffectFunction1Wrapper[T, R](eff: SideEffect[T => R]){
//    def composeEffect[R2](eff2: SideEffect[R => R2]): SideEffect[T => R2] = SideEffect(eff2.execute compose eff.execute)
    def apply(t: T): SideEffect[R] = sideEffect{ eff.execute apply t }
  }

  implicit class SideEffectResultingFunction1Wrapper[T, R](f: T => SideEffect[R]){
    def composeEffect[T2](f2: T2 => T): SideEffect[T2 => R] = sideEffect{ f andThen (_.execute) compose f2 }
    def flatCompose[T2](f2: T2 => SideEffect[_ <: T]): SideEffect[T2 => R] = sideEffect{ f andThen (_.execute) compose f2.andThen(_.execute) }
  }

//  implicit class Function1ToSideEffectWrapper[T, R](f: T => R){
//    def composeEffect[R2](eff: SideEffect[R => R2]): SideEffect[T => R2] = SideEffect(eff.execute compose f)
//  }

}

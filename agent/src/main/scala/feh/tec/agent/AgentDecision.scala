package feh.tec.agent

object AgentDecision{
  trait ExtendedDecision[D]{
    def decision: D
  }

  trait DecisionStrategy[Action, DArg, D]{ self =>
    type DExtended <: ExtendedDecision[D] 
    def decide: DArg => DExtended 

    def failsafe(isFail: DExtended => Boolean,
                 onFail: (DArg, DExtended) => DExtended): FailsafeDecisionStrategy[Action, DArg, D, self.type]
  }

  trait FailsafeDecisionStrategy[Action, DArg, D, G <: DecisionStrategy[Action, DArg, D]] extends DecisionStrategy[Action, DArg, D]{
    type DExtended = G#DExtended

    def guardedStrategy: G
    def strategyFailed: G#DExtended => Boolean
    def onFailure: (DArg, G#DExtended) => DExtended

    def decide: (DArg) => DExtended = { arg =>
      val g = guardedStrategy.decide(arg)
      if(strategyFailed(g)) onFailure(arg, g)
      else g
    }
  }

  case class GenericFailsafeDecisionStrategy[Action, DArg, D, G <: DecisionStrategy[Action, DArg, D]](guardedStrategy: G, strategyFailed: G#DExtended => Boolean, onFailure: (DArg, G#DExtended) => G#DExtended)
    extends FailsafeDecisionStrategy[Action, DArg, D, G]
  {
    self =>

    def failsafe(isFail: DExtended => Boolean,
                 onFail: (DArg, DExtended) => DExtended): FailsafeDecisionStrategy[Action, DArg, D, self.type] =
      GenericFailsafeDecisionStrategy[Action, DArg, D, this.type](this, isFail, onFail)
  }


  trait ExplainedAction[Action]{
    type Explanation

    def action: Action
    def explain: Explanation
  }

  trait CriteriaReasonedExplanation[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                    Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                    Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                    M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
  {
    type Explanation = (Set[M#CriterionValue], M#Measure)

    def criteria: Set[M#CriterionValue]
    def measure: M#Measure
    def message: CriteriaMessage[M#CriterionValue]

    def explain: Explanation = criteria -> measure
  }

  case class CriteriaMessage[CriterionValue](beforeCriteria: String = null, afterCriteria: String = null){
    def beforeOpt = Option(beforeCriteria)
    def afterOpt = Option(afterCriteria)
  }

  case class CriteriaReasonedDecisions[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                       Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                       Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                       M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
    (action: Seq[Action], criteria: Set[M#CriterionValue], measure: M#Measure, message: CriteriaMessage[M#CriterionValue] = CriteriaMessage[M#CriterionValue]())
      extends ExplainedAction[Seq[Action]] with CriteriaReasonedExplanation[Position, EnvState, EnvGlobal, Action, Env, Exec, M]
  {
    def toSeq = action.map(d => CriteriaReasonedDecision[Position, EnvState, EnvGlobal, Action, Env, Exec, M](d, criteria, measure, message))
  }


  case class CriteriaReasonedDecision[Position, EnvState, EnvGlobal, Action <: AbstractAction,
                                      Env <: Environment[Position, EnvState, EnvGlobal, Action, Env],
                                      Exec <: AgentExecutionLoop[Position, EnvState, EnvGlobal, Action, Env],
                                      M <: AgentPerformanceMeasure[Position, EnvState, EnvGlobal, Action, Env, M]]
    (action: Action, criteria: Set[M#CriterionValue], measure: M#Measure, message: CriteriaMessage[M#CriterionValue] = CriteriaMessage[M#CriterionValue]())
      extends ExplainedAction[Action] with CriteriaReasonedExplanation[Position, EnvState, EnvGlobal, Action, Env, Exec, M]
}

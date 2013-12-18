package feh.tec.agent


trait AgentMeasure[Env <: Environment[Env], M <: AgentMeasure[Env, M]]
{
  self: M =>

  type Arguments
  type Measure
  implicit val measureIsNumeric: Numeric[M#Measure]

  def measure(arg: M#Arguments): M#Measure

  type EnvRef = EnvironmentRef[Env]
  type Ag = IndecisiveAgent[Env]
  type Snapshot = EnvironmentSnapshot[Env]
}

trait AgentPerformanceMeasure[Env <: Environment[Env], M <: AgentPerformanceMeasure[Env, M]]
  extends AgentMeasure[Env, M]
{
  self: M =>

  type CriterionArgs

  type Criterion <: AgentPerformanceMeasureCriterion[Env, M]
  final type Criteria = Seq[M#Criterion]

  type CriterionValue <: AgentPerformanceMeasureCriterionValue[Env, M]
  type CriteriaValue = Seq[M#CriterionValue]
}

trait AgentPerformanceMeasureCriterion[Env <: Environment[Env], M <: AgentPerformanceMeasure[Env, M]]{
  def name: String
  def assess:  M#CriterionArgs => M#Measure
}

trait AgentPerformanceMeasureCriterionValue[Env <: Environment[Env], M <: AgentPerformanceMeasure[Env, M]]{
  def name: String
  def value:  M#Measure
}

trait StatelessAgentPerformanceMeasure[Env <: Environment[Env], M <: StatelessAgentPerformanceMeasure[Env, M]]
  extends AgentPerformanceMeasure[Env, M]
{
  self: M =>

  type CriterionArgs = M#Snapshot
  type Criterion = StatelessAgentPerformanceMeasure.Criterion[Env, M]
  type CriterionValue = StatelessAgentPerformanceMeasure.CalculatedCriterion[Env, M]
  type Arguments = (M#CriterionArgs, M#Criteria)


  def performanceDebug = false

  def performance(snapshot: M#Snapshot)(implicit criteria: M#Criteria): M#CriteriaValue =
    (Seq.empty[M#CriterionValue] /: criteria){ (acc, criterion) =>
      val m = criterion.calculate(snapshot)
      if(performanceDebug) println(s"[Measure Performance] ${criterion.name}: $m}")
      acc :+ m
    }

  def measure(arg: M#Arguments): M#Measure = performance(arg._1)(arg._2).map(_.value).sum(measureIsNumeric)
}

class StatelessAgentPerformanceDoubleMeasure[Env <: Environment[Env], M <: StatelessAgentPerformanceDoubleMeasure[Env, M]]
  extends StatelessAgentPerformanceMeasure[Env, M]
{
  self: M =>

  type Measure = Double
  implicit val measureIsNumeric = Numeric.DoubleIsFractional
}


object StatelessAgentPerformanceMeasure{
  case class Criterion[Env <: Environment[Env], M <: StatelessAgentPerformanceMeasure[Env, M]]
    (name: String, assess: M#Snapshot => M#Measure) extends AgentPerformanceMeasureCriterion[Env, M]
  {
    def calculate(sn: M#Snapshot): CalculatedCriterion[Env, M] =
      CalculatedCriterion[Env, M](name, assess(sn))
  }
  case class CalculatedCriterion[Env <: Environment[Env], M <: AgentPerformanceMeasure[Env, M]]
    (name: String, value: M#Measure) extends AgentPerformanceMeasureCriterionValue[Env, M]
}
package feh.tec.agent

trait AgentMeasure {
  type MeasureUnit
  def measureNumeric: Numeric[MeasureUnit]

  def measuring: Agent => MeasureUnit
}


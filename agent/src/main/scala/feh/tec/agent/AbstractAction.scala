package feh.tec.agent

trait AbstractAction {

}

trait AbstractResultAction[+R] extends AbstractAction{
  def result: R
}
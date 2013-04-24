package playground.poker.repl.commands

import playground.poker.Environment

case class MultiCommand(commands: List[ReplCommand]) extends ReplCommand {
  override def apply(env: Environment) = {
    def execNext(commands: List[ReplCommand], env: Environment): Option[Environment] = commands match {
      case hd :: tl => hd(env) match {
        case Some(newEnv) => execNext(tl, newEnv)
        case None => None
      }
      case _ => Some(env)
    }
    execNext(commands, env)
  }
}

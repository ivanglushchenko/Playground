package playground.poker.repl.commands

import playground.poker.Environment

case object ClearCommand extends ReplCommand {
  override def apply(env: Environment) = Some(Environment())
}

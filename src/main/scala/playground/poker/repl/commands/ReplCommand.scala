package playground.poker.repl.commands

import playground.poker._

abstract class ReplCommand extends Function[Environment, Option[Environment]]

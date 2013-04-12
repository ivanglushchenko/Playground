package playground.poker

abstract sealed class ReplCommand
case object NopCommand extends ReplCommand
case object ExitCommand extends ReplCommand
case class HandCommand(hand: Hand) extends ReplCommand
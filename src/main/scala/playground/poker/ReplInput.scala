package playground.poker

import scala.util.parsing.combinator._

object ReplInput extends RegexParsers {
  // Basic templates
  val rank: Parser[Rank] = "[2-9TJQKAtjqka]{1,1}".r ^^ { t => Rank.unapply(t).get }
  val suit: Parser[Suit] = "[CDHScdhs]{1,1}".r ^^ { Suit.unapply(_).get }
  val card: Parser[Card] = rank ~ suit ^^ { case rank ~ suit => new Card(rank, suit) }
  val hand: Parser[Hand] = card ~ opt(hand) ^^ { case card ~ next => toHand(List(card), next) }

  // Commands
  val exit: Parser[ReplCommand] = "exit" ^^ { _ => ExitCommand }
  val about: Parser[ReplCommand] = "help" ^^ { _ => HelpCommand }
  val prob: Parser[ReplCommand] = "prob" ~ opt("?") ^^ { _ => ProbabilityCommand }
  val addHand: Parser[ReplCommand] = "hand" ~> "[1-9]?".r ~ hand ^^ { case name ~ hand => AddHandCommand(name, hand) }
  val askHand: Parser[ReplCommand] = "hand?" ^^ { _ => AskHandCommand }
  val addOpenCard: Parser[ReplCommand] = "+" ~> card ^^ { AddOpenCardCommand(_) }

  val cmd: Parser[ReplCommand] = (
      exit
      | about
      | prob
      | addHand
      | askHand
      | addOpenCard
      | hand ^^ { x => AddHandCommand("", x) })

  def parseHand(line: String): Option[Hand] = parseAll(hand, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }

  def parseCmd(line: String): Option[ReplCommand] = parseAll(cmd, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }

  def toHand(cards: List[Card], nextHand: Option[Hand]): Hand = nextHand match {
    case Some(hand) => new Hand(cards ::: hand.cards)
    case None => new Hand(cards)
  }
}
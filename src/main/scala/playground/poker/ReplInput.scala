package playground.poker

import scala.util.parsing.combinator._

object ReplInput extends RegexParsers {
  def toHand(cards: List[Card], nextHand: Option[Hand]): Hand = nextHand match {
    case Some(hand) => new Hand(cards ::: hand.cards)
    case None => new Hand(cards)
  }

  val cmdExit: Parser[ReplCommand] = "exit" ^^ { _ => ExitCommand }
  val rank: Parser[Rank] = """[2-9TJQKAtjqka]{1,1}""".r ^^ { t => Rank.unapply(t).get }
  val suit: Parser[Suit] = """[CDHScdhs]{1,1}""".r ^^ { Suit.unapply(_).get }
  val card: Parser[Card] = rank ~ suit ^^ { case rank ~ suit => new Card(rank, suit) }
  val hand: Parser[Hand] = card ~ opt(hand) ^^ { case card ~ next => toHand(List(card), next) }
  val cmd: Parser[ReplCommand] = cmdExit | hand ^^ { x => HandCommand(x) }

  def parseHand(line: String): Option[Hand] = parseAll(hand, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }

  def parseCmd(line: String): Option[ReplCommand] = parseAll(cmd, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }
}
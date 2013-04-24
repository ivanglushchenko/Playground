package playground.poker.repl

import playground.poker.cards._
import playground.poker.repl.commands._
import scala.util.parsing.combinator._

object ReplInput extends RegexParsers {
  // Basic templates
  val rank: Parser[Rank] = "[2-9TJQKAtjqka]{1,1}".r ^^ {
    _.toUpperCase match {
      case "A" => Ace
      case "K" => King
      case "Q" => Queen
      case "J" => Jack
      case "T" => NumRank(10)
      case n => NumRank(n.toInt)
    }
  }
  val suit: Parser[Suit] = "[CDHScdhs]{1,1}".r ^^ {
    _.toUpperCase match {
      case "H" => Hearts
      case "D" => Diamonds
      case "S" => Spades
      case "C" => Clubs
    }
  }
  val card: Parser[Card] = rank ~ suit ^^ { case rank ~ suit => new Card(rank, suit) }
  val hand: Parser[Hand] = card ~ opt(hand) ^^ { case card ~ next =>
    next match {
      case Some(hand) => new Hand(card :: hand.cards)
      case None => new Hand(List(card))
    }
  }

  // Commands
  val exit: Parser[ReplCommand] = "exit" ^^ { _ => ExitCommand }
  val about: Parser[ReplCommand] = "help" ^^ { _ => HelpCommand }
  val prob: Parser[ReplCommand] = "prob" ~ opt("?") ^^ { _ => ProbabilityCommand } | "?" ^^ { _ => ProbabilityCommand }
  val setHand: Parser[ReplCommand] = "hand" ~> "[1-9]?".r ~ hand ^^ { case name ~ hand => SetHandCommand(name, hand) }
  val askHand: Parser[ReplCommand] = "hand?" ^^ { _ => AskHandCommand }
  val addCards: Parser[ReplCommand] = "+" ~> hand ^^ { hand => AddCardsCommand(hand.cards) }
  val test: Parser[ReplCommand] = "test" ~> opt(hand) ^^ { TestCommand(_) }
  val clear: Parser[ReplCommand] = "cl" ^^ { _ => ClearCommand }

  val allCommands: Parser[ReplCommand] = (
      exit
      | about
      | prob
      | setHand
      | askHand
      | addCards
      | test
      | clear
      | hand ~ opt("?") ^^ { case x ~ q => q match {
        case None => SetHandCommand("", x)
        case _ => MultiCommand(List(SetHandCommand("", x), ProbabilityCommand))
      } })

  def parseRank(line: String): Option[Rank] = parseAll(rank, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }

  def parseSuit(line: String): Option[Suit] = parseAll(suit, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }

  def parseCard(line: String): Option[Card] = parseAll(card, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }

  def parseHand(line: String): Option[Hand] = parseAll(hand, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }

  def parseCmd(line: String): Option[ReplCommand] = parseAll(allCommands, line) match {
    case Success(t, _) => Some(t)
    case _ => None
  }
}
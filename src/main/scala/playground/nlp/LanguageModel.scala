package playground.nlp

abstract class LanguageModelItem {}

abstract class Word extends LanguageModelItem {}

case class WordInstance(word: String) extends Word {
  override def toString(): String = word
}

case object RareWord extends Word {
  override def toString(): String = "_RARE_"
}

case object NumericRareWord extends Word {
  override def toString(): String = "_NUMRARE_"
}

case object AllCapsRareWord extends Word {
  override def toString(): String = "_CAPSRARE_"
}

case object LastCapRareWord extends Word {
  override def toString(): String = "_LASTCAPRARE_"
}

case object Empty extends Word {
  override def toString(): String = "_"
}

object Word {
  def apply(word: String): Word = WordInstance(word)
}

abstract class Tag extends LanguageModelItem {}

case object O extends Tag {
  override def toString(): String = "O"
}

case object Gene extends Tag {
  override def toString(): String = "I-GENE"
}

case object Star extends Tag {
  override def toString(): String = "*"
}

case object Stop extends Tag {
  override def toString(): String = "STOP"
}

object Tag {
  def parse(tag: String): Tag = tag match {
    case "O" => O
    case "I-GENE" => Gene
    case _ => throw new Exception("Can't recognize tag " + tag)
  }
}

case class Trigram(val t1: LanguageModelItem, val t2: Tag, val t3: Tag) extends LanguageModelItem {
  override def toString(): String = t1.toString() + " " + t2 + " " + t3
}

object Trigram {
  def apply(t1: Tag, t2: Tag, t3: Tag): Trigram = new Trigram(t1, t2, t3)
}

abstract class NonTerminal {}

case class UnaryRule(val tag: String, val word: String) extends NonTerminal {
  override def toString() = "[\"" + tag + "\", \"" + word + "\"]"
}

case class BinaryRule(val from: String, val to1: NonTerminal, val to2: NonTerminal) extends NonTerminal {
  override def toString() = "[\"" + from + "\", " + to1 + ", " + to2 + "]"
}
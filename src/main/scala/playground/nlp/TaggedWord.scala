package playground.nlp

case class TaggedWord(word: Word, tag: Tag, count: Int) {
  def updateWord(word: Word): TaggedWord = TaggedWord(word, tag, count)
  override def toString(): String = tag + " " + word + ": " + count
}

object TaggedWord {
  def apply(word: Word, tag: Tag): TaggedWord = TaggedWord(word, tag, 1)
}

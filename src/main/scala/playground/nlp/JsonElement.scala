package playground.nlp

abstract class JsonElement {
  def getValue(): String = throw new Exception()
}

case class JsonArray(val items: List[JsonElement]) extends JsonElement {
  override def toString() = "[" + items.mkString(", ") + "]"
}

case class JsonString(val value: String) extends JsonElement {
  override def toString() = "\"" + value + "\""
  override def getValue(): String = value
}

object JsonElement {
  def parse(line: String): JsonElement = {
    def parseNextChar(i: Int): (JsonElement, Int) = {
      if (line.size <= 1) null
      else {
        line(i) match {
          case '[' => parseArray(i)
          case '"' => parseString(i)
        }
      }
    }

    def parseArray(i: Int): (JsonArray, Int) = {
      def parseElements(i: Int, elements: List[JsonElement]): (List[JsonElement], Int) = {
        val (nextEl, nextIndex) = parseNextChar(i)
        val nextElements = nextEl :: elements
        if (line(nextIndex) == ',' && line(nextIndex + 1) == ' ') parseElements(nextIndex + 2, nextElements)
        else if (line(nextIndex) == ',') parseElements(nextIndex + 1, nextElements)
        else if (line(nextIndex) == ']') (nextElements.reverse, nextIndex)
        else throw new Exception()
      }
      val (elements, nextIndex) = parseElements(i + 1, List())
      (JsonArray(elements), nextIndex + 1)
    }
    def parseString(i: Int): (JsonString, Int) = {
      val closingBracket = 0//line.substring(i + 1).findIndexOf(_ == '"')
      val value = line.substring(i + 1, i + 1 + closingBracket)
      (JsonString(value), i + 1 + closingBracket + 1)
    }

    parseNextChar(0)._1
  }
}
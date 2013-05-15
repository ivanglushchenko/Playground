package playground.nlp

import scala.io._
import java.io._

class Tagger(fileName: String) {
  def parseLine(line: String): TaggedWord =
    if (line == "") TaggedWord(Empty, Stop)
    else {
      val parts = line.split(" ")
      TaggedWord(Word(parts(0)), Tag.parse(parts(1)))
    }

  val lines = Source.fromFile(fileName).getLines.map(_.split(" ")) toList
  val tags = lines.map(parts => if (parts.length == 2) TaggedWord(Word(parts(0)), Tag.parse(parts(1))) else TaggedWord(Empty, Stop))

  def groupWordTags(list: List[TaggedWord]): List[TaggedWord] = {
    (for {
      tagGroup <- list groupBy(_.tag)
      val (tag, words) = tagGroup
      if tag != Stop
      word <- words groupBy(_.word)
    } yield TaggedWord(word._1, tag, word._2.map(_.count).sum)) toList
  }

  val wordTags = groupWordTags(tags)

  def asRare(wt: TaggedWord): TaggedWord = {
    val w = wt.word.toString
    if (w.foldLeft(false)((acc, el) => acc || el.isDigit)) wt.updateWord(NumericRareWord)
    else if (w.foldLeft(true)((acc, el) => acc && el.isUpper)) wt.updateWord(AllCapsRareWord)
    else if (w.last.isUpper) wt.updateWord(LastCapRareWord)
    else
      wt.updateWord(RareWord)
  }

  val smoothedWordTags = {
    def rarefy(wt: TaggedWord): TaggedWord =
      if (wt.count < 5) asRare(wt)
      else wt
    groupWordTags(wordTags map rarefy)
  }

  val emissionParams = {
    val tagOCount = tags count(_.tag == O)
    val tagGeneCount = tags count(_.tag == Gene)
    val tagsMap: Map[Tag, Float] = Map(O -> tagOCount, Gene -> tagGeneCount)

    def toTagMap(list: List[TaggedWord]): Map[Tag, Float] = {
      list.map(wt => (wt.tag, wt.count.toFloat / tagsMap(wt.tag))).toMap
    }

    smoothedWordTags.groupBy(_.word).map(g => (g._1, toTagMap(g._2)))
  }

  def printWordTags() =  for { wt <- wordTags } println(wt)

  def emitTag(word: Word) = {
    val w = if (emissionParams.contains(word)) word else RareWord
    emissionParams(w).toList.sortBy(- _._2).head._1
  }

  def e(word: String, tag: Tag): Float = {
    val t = Word(word)
    val w = if (emissionParams.contains(t)) t else asRare(TaggedWord(t, tag)).word
    if (emissionParams(w).contains(tag)) emissionParams(w)(tag)
    else 0
  }

  def saveWordTagsTo (fileName: String) = {
    printToFile(fileName) {
      writer => for { wt <- smoothedWordTags } writer.println(wt)
    }
  }

  def tag(in: String, out: String) {
    val lines = Source.fromFile(in).getLines
    printToFile(out) {
      writer => for { word <- lines } if (word == "") writer.println("") else writer.println(word + " " + emitTag(Word(word)))
    }
  }

  def printToFile(fileName: String)(op: java.io.PrintWriter => Unit) {
    val file = new File(fileName)
    val p = new java.io.PrintWriter(file)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  val trigrams = {
    def moveToNextWord(lines: List[TaggedWord], prePrev: Tag, prev: Tag, trigrams: List[Trigram]): List[Trigram] = lines match {
      case hd :: tl =>
        val nextTrigram = Trigram(prePrev, prev, hd.tag)
        if (hd.tag == Stop) moveToNextWord(tl, Star, Star, nextTrigram :: trigrams)
        else moveToNextWord(tl, prev, hd.tag, nextTrigram :: trigrams)
      case _ =>
        trigrams.reverse
    }

    moveToNextWord(tags, Star, Star, List())
  }

  val unigramCounts = {
    trigrams.map(t => t.t3).filter(_ != Stop).groupBy(t => t).map(t => (t._1, t._2.length))
  }

  val bigramCounts = {
    def toBigrams(t: Trigram): List[(Tag, Tag)] = t match {
      case Trigram(Star, Star, x) => List[(Tag, Tag)]((Star, Star), (Star, x))
      case Trigram(_, x1, x2) => List((x1, x2))
      case _ => List()
    }

    trigrams.flatMap(toBigrams).groupBy(t => t).map(t => (t._1, t._2.length))
  }

  def bigramCount(t1: Tag, t2: Tag): Float = {
    val pair = (t1, t2)
    bigramCounts(pair)
  }

  val trigramCounts = trigrams.groupBy(t => (t.t1, t.t2, t.t3)).map(t => (t._1, t._2.length))

  def trigramCount(t1: Tag, t2: Tag, t3: Tag): Float = {
    val triple = (t1, t2, t3)
    if (trigramCounts.contains(triple)) trigramCounts(triple)
    else 0
  }

  def q(t1: Tag, t2: Tag, t3: Tag): Float = trigramCount(t1, t2, t3) / bigramCount(t1, t2)

  var allowedFixes = 450

  def tagSentence(words: List[String]): List[(String, Tag)] = {
    def toIndex(t: Tag) = t match {
      case Star => 0
      case O => 1
      case Gene => 2
      case Stop => 3
    }

    def s(i: Int): List[Tag] = if (i <= 0) List(Star) else List(O, Gene)

    val a = Array.fill(words.length + 1, 4, 4)(0.0)
    val b = Array.fill(words.length + 1, 4, 4)(Stop.asInstanceOf[Tag])
    a(0)(toIndex(Star))(toIndex(Star)) = 1

    for {
      k <- 1 to words.length
      u <- s(k - 1)
      v <- s(k)
    } {
      val allPaths =
        for {
          w <- s(k - 2)
          val p1 = a(k - 1)(toIndex(w))(toIndex(u))
          val p2 = q(w, u, v)
          val p3 = e(words(k - 1), v)
          val p = a(k - 1)(toIndex(w))(toIndex(u)) * q(w, u, v) * e(words(k - 1), v)
        } yield {
          //println("    p[w=" + w + "] = " + p + ", a = " + p1 + ", q = " + p2 + ", e[" + words(k - 1) + "," + v + "] = " + p3)
          (p, w)
        }
      val max = allPaths.sortBy(t => - t._1).head
      a(k)(toIndex(u))(toIndex(v)) = max._1
      b(k)(toIndex(u))(toIndex(v)) = max._2
      //println("looking at '" + words(k - 1) + "' u=" + u + " v=" + v + " max=" + max)
    }

    val endings =
      for {
        u <- s(words.length - 1)
        v <- s(words.length)
      } yield (u, v, a(words.length)(toIndex(u))(toIndex(v)) * q(u, v, Stop))
    val (u, v, _) = endings.sortBy(t => - t._3).head

    def reconstructTagSequence(k: Int, u: Tag, v: Tag, seq: List[Tag]): List[Tag] =
      if (k < 1) seq
      else {
        val t = b(k + 2)(toIndex(u))(toIndex(v))
        reconstructTagSequence(k - 1, t, u, t :: seq)
      }

    val tags = reconstructTagSequence(words.length - 2, u, v, List(u, v))
    val res = words.zip(tags)

    def fixTags(pair: (String, Tag)): (String, Tag) = {
      def has(f: Char => Boolean): Boolean = pair._1.foldLeft(false)((acc, el) => acc || f(el))
      if (has(_.isDigit) && has(_.isLetter) && allowedFixes > 0) {
        allowedFixes = allowedFixes - 1
        (pair._1, Gene)
      }
      else pair
    }

    res map fixTags
    //res
  }

  def tagViterbi(in: String, out: String) {
    val lines = Source.fromFile(in).getLines() toList

    def foreachSentence(handler: List[String] => Unit) {
      def loop(lines: List[String], acc: List[String]): Unit =
        if (lines.isEmpty) { if (acc.length > 0) handler(acc) }
        else {
          if (lines.head == "") {
            handler(acc.reverse)
            loop(lines.tail, List())
          } else loop(lines.tail, lines.head :: acc)
        }
      loop(lines, List())
    }

    printToFile(out) {
      writer => foreachSentence {
        sentence => {
          for {
            pair <- tagSentence(sentence)
            val (word, tag) = pair
          } if (word == "") writer.println("") else writer.println(word + " " + tag)
          writer.println("")
        }
      }
    }
  }

  /*
  printToFile("c:\\Source\\NLP\\hw1\\uniCounts") {
    writer => writer.println(unigramCounts)
  }

  printToFile("c:\\Source\\NLP\\hw1\\biCounts") {
    writer => writer.println(bigramCounts)
  }

  printToFile("c:\\Source\\NLP\\hw1\\triCounts") {
    writer => writer.println(trigramCounts)
  }

  printToFile("c:\\Source\\NLP\\hw1\\tags") {
    writer => writer.println(tags)
  }

  printToFile("c:\\Source\\NLP\\hw1\\trigrams") {
    writer => writer.println(trigrams)
  }
  */
}
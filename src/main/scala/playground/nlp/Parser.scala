package playground.nlp

import java.io._
import scala.io._

class Parser {
  def loadCountsFromFile(fileName: String): List[NonTerminal] = {
    Source.fromFile(fileName).getLines().map(parse).toList
  }

  def parse(line: String): NonTerminal = {
    def parseJson(el: JsonElement): NonTerminal = el match {
      case JsonArray(a) =>
        if (a.length == 2) UnaryRule(a(0).getValue(), a(1).getValue())
        else BinaryRule(a(0).getValue(), parseJson(a(1)), parseJson(a(2)))
      case _ => throw new Exception("")
    }

    parseJson(JsonElement.parse(line))
  }

  //val trainingSet = loadCountsFromFile("c:\\Source\\NLP\\hw2\\parse_train.dat")
  val trainingSet = loadCountsFromFile("c:\\Source\\NLP\\hw2\\parse_train_vert.dat")

  def collectWords(nt: NonTerminal): List[String] = {
    def loop(nt: NonTerminal, acc: List[String]): List[String] = nt match {
      case UnaryRule(_, word) => word :: acc
      case BinaryRule(_, nt1, nt2) => loop(nt2, loop(nt1, acc))
    }
    loop(nt, List())
  }

  val rareWords =
    trainingSet
      .flatMap(collectWords)
      .groupBy(w => w)
      .filter(_._2.size < 5)
      .map(t => t._1)
      .toSet

  def replaceRareWords(nt: NonTerminal): NonTerminal = nt match {
    case UnaryRule(tag, word) if rareWords.contains(word) => UnaryRule(tag, "_RARE_")
    case UnaryRule(_, _) => nt
    case BinaryRule(from, to1, to2) => BinaryRule(from, replaceRareWords(to1), replaceRareWords(to2))
  }

  val smoothedTrainingSet = trainingSet map replaceRareWords

  printToFile("c:\\Source\\NLP\\hw2\\parser_train_rare.dat") {
    writer => for { tree <- smoothedTrainingSet } writer.println(tree)
  }

  def printToFile(fileName: String)(op: java.io.PrintWriter => Unit) {
    val file = new File(fileName)
    val p = new java.io.PrintWriter(file)
    try {
      op(p)
    } catch {
      case e: Throwable => println(e)
    } finally {
      p.close()
    }
  }

  val nonTerminals = {
    def collectNonTerminals(nt: NonTerminal): List[String] = {
      def loop(nt: NonTerminal, acc: List[String]): List[String] = nt match {
        case UnaryRule(tag, _) => tag :: acc
        case BinaryRule(from, nt1, nt2) => loop(nt2, loop(nt1, from :: acc))
      }
      loop(nt, List())
    }

    smoothedTrainingSet
      .flatMap(collectNonTerminals)
      .groupBy(w => w)
      .map(g => (g._1, g._2.size))
  }

  printToFile("c:\\Source\\NLP\\hw2\\nonTerminals.dat") {
    writer => for { nt <- nonTerminals } writer.println(nt)
  }

  val words = {
    def collectWordtags(nt: NonTerminal): List[String] = {
      def loop(nt: NonTerminal, acc: List[String]): List[String] = nt match {
        case UnaryRule(tag, word) => word :: acc
        case BinaryRule(from, nt1, nt2) => loop(nt2, loop(nt1, acc))
      }
      loop(nt, List())
    }
    trainingSet
      .flatMap(collectWordtags)
      .toSet
  }

  val unigrams = {
    def collectUnigrams(nt: NonTerminal): List[(String, String)] = {
      def loop(nt: NonTerminal, acc: List[(String, String)]): List[(String, String)] = nt match {
        case UnaryRule(tag, word) => (tag, word) :: acc
        case BinaryRule(from, nt1, nt2) => loop(nt2, loop(nt1, acc))
      }
      loop(nt, List())
    }
    smoothedTrainingSet
      .flatMap(collectUnigrams)
      .groupBy(_._1)
      .map(g => (g._1, g._2.size))
  }

  val wordtags = {
    def collectWordtags(nt: NonTerminal): List[(String, String)] = {
      def loop(nt: NonTerminal, acc: List[(String, String)]): List[(String, String)] = nt match {
        case UnaryRule(tag, word) => (tag, word) :: acc
        case BinaryRule(from, nt1, nt2) => loop(nt2, loop(nt1, acc))
      }
      loop(nt, List())
    }

    smoothedTrainingSet
      .flatMap(collectWordtags)
      .groupBy(_._2)
      .map(g => (g._1, g._2.groupBy(g => g._1).map(g => (g._1, g._2.size))))
  }

  printToFile("c:\\Source\\NLP\\hw2\\wordtags.dat") {
    writer => for { nt <- wordtags } writer.println(nt)
  }

  def ruleName(r: NonTerminal): String = r match {
    case UnaryRule(tag, _) => tag
    case BinaryRule(from, _, _) => from
  }

  val trigrams = {
    def collectBinaryRules(nt: NonTerminal): List[(String, String, String)] = {
      def loop(nt: NonTerminal, acc: List[(String, String, String)]): List[(String, String, String)] = nt match {
        case UnaryRule(tag, word) => acc
        case BinaryRule(from, r1, r2) => {
          val n1 = ruleName(r1)
          val n2 = ruleName(r2)
          loop(r1, loop(r2, (from, n1, n2) :: acc))
        }
      }
      loop(nt, List())
    }

    val plainList = smoothedTrainingSet
      .flatMap(collectBinaryRules)

    printToFile("c:\\Source\\NLP\\hw2\\trigramsRaw.dat") {
      writer => for { t <- plainList } writer.println(t)
    }

    plainList
      .groupBy(_._1)
      .map(g => (g._1, g._2.groupBy(g => g._2).map(g => (g._1, g._2.groupBy(g => g._3).map(g => (g._1, g._2.size))))))
  }

  def qForUnaryRule(tag: String, word: String): Float = {
    def countUnaryRules(tag: String, word: String): Int = {
      val wordOrRare = if (wordtags.contains(word)) word else "_RARE_"
      if (wordtags.contains(wordOrRare) && wordtags(wordOrRare).contains(tag)) wordtags(wordOrRare)(tag) else 0
    }
    //println("qForUnaryRule(" + tag + ", " + word + ")")
    //val wordOrRare = if (words.contains(word)) word else "_RARE_"
    //if (!words.contains(word)) qForUnaryRule(tag, "_RARE_")
    //else
    countUnaryRules(tag, word).toFloat / nonTerminals(tag).toFloat
  }

  def qForBinaryRule(from: String, to1: String, to2: String): Float = {
    def countBinaryRules(from: String, to1: String, to2: String): Int =
      if (trigrams.contains(from) && trigrams(from).contains(to1) && trigrams(from)(to1).contains(to2)) trigrams(from)(to1)(to2)
      else 0
    countBinaryRules(from, to1, to2).toFloat / nonTerminals(from).toFloat
  }

  printToFile("c:\\Source\\NLP\\hw2\\unaryRules.dat") {
    writer => for { nt <- wordtags } writer.println(nt)
  }

  printToFile("c:\\Source\\NLP\\hw2\\trigrams.dat") {
    writer => writer.println(trigrams)
  }

  def cky(sentence: List[String]): NonTerminal = {
    val allNonTerminals = nonTerminals.map(t => t._1).toList

    val n = sentence.size
    val a = Array.fill(n, n)(Map.empty[String, Double].withDefaultValue(0.0))
    val b = Array.fill(n, n)(Map.empty[String, NonTerminal])

    for {
      i <- 0 until n
      x <- allNonTerminals
      p = qForUnaryRule(x, sentence(i))
      if p > 0
    } {
      //println("init: " + sentence(i) + "[" + x + "] = " + p)
      //set(i, i, x, p)
      a(i)(i) = a(i)(i).updated(x, p)
      b(i)(i) = b(i)(i).updated(x, UnaryRule(x, sentence(i)))
    }

    for {
      l <- 1 until n
      i <- 0 until (n - l)
      j = i + l
    } {
      val subPhrase = sentence.drop(i).take(l + 1).mkString(" ")
      //println("phrase: " + subPhrase + " (i=" + (i + 1) + ", j=" + (j + 1) + ")")

      for (s <- i until j) {
        //println("  split: [" + (i + 1) + "," + (s + 1) + "], [" + (s + 2) + ", " + (j + 1) + "]")

        for {
          x <- allNonTerminals
          if trigrams.contains(x)
          (y, nextProductions) <- trigrams(x)
          (z, _) <- nextProductions
        } {
          //println("    " + x + " -> " + y + " " + z + " = " + qForBinaryRule(x, y, z) * get(i, s, y) * get(s + 1, j, z) + " (q=" + qForBinaryRule(x, y, z) + ", g1=" + get(i, s, y) + ", g2=" + get(s + 1, j, z))
        }

        for {
          x <- allNonTerminals
          if trigrams.contains(x)
          (y, nextProductions) <- trigrams(x)
          if a(i)(s)(y) > 0
          (z, _) <- nextProductions
          if a(s + 1)(j)(z) > 0
        } {
          //println("    " + x + " -> " + y + " " + z + " = " + qForBinaryRule(x, y, z) * get(i, s, y) * get(s + 1, j, z))
        }
      }

      for {
        x <- allNonTerminals
        if trigrams.contains(x)
      } {
        val possibleProductions =
          for {
            s <- i until j
            (y, nextProductions) <- trigrams(x)
            if a(i)(s)(y) > 0
            (z, _) <- nextProductions
            if a(s + 1)(j)(z) > 0
            p = qForBinaryRule(x, y, z) * a(i)(s)(y) * a(s + 1)(j)(z)
            if p > 0
          } yield (p, y, z, s)
        if (!possibleProductions.isEmpty) {
          val max = possibleProductions.maxBy(_._1)
          //set(i, j, x, max._1)
          //a(0)(1) = a(0)(1).updated("a", 1)
          //a(0)(1) = a(0)(1).updated("a", 2)
          //val tt = a(0)(1)("a")
          a(i)(j) = a(i)(j).updated(x, max._1)
          //val ret = a(i)(j)(x)
          //if (get(i, j, x) != max._1) println("" + ret + " != " + max._1)
          //println("    pi(" + i + ", " + j + ", " + x + ") -> " + max._2 + " " + max._3 + " = " + max._1)
          b(i)(j) = b(i)(j).updated(x, BinaryRule(x, b(i)(max._4)(max._2),  b(max._4 + 1)(j)(max._3)))

        }
      }

      /*
      for {
        x <- allNonTerminals
        if trigrams.contains(x)
        productions = trigrams(x).toList.flatMap(t => t._2.map(t2 => (t._1, t2._1)))
      } {
        val possibleProductions =
          for {
            (y, z) <- productions
            s <- i until j
            p = qForBinaryRule(x, y, z) * get(i, s, y) * get(s + 1, j, z)
            if p > 0
          } yield (p, y, z, s)
        if (!possibleProductions.isEmpty) {
          val max = possibleProductions.maxBy(_._1)
          //println("  productions for " + x + ": " + productions.size)
          set(i, j, x, max._1)
          b(i)(j)(ind(x)) = (max._2, max._3, max._4)
          //println("proc " + i + " to " + j + ": " + subPhrase + ", " + l + " for " + x + " => " + max._2 + " " + max._3)
        }
      }
      */
    }




    //println("t5: " + qForUnaryRule("NUM", "monetary"))
    //println("t6: " + qForUnaryRule("ADJ", "monetary"))
    //println("t7: " + qForUnaryRule("NOUN", "monetary"))


    //println("res:  " + a(0)(n - 1)(ind("SBARQ")))
    //println("from: " + b(0)(n - 1)(ind("SBARQ")))

    /*
        def reconstructTree(i: Int, j: Int, tag: String): NonTerminal = {
          if (i == j) UnaryRule(tag, sentence(i))
          else {
            val (y, z, s) = b(i)(j)(ind(tag))
            BinaryRule(tag, reconstructTree(i, s, y), reconstructTree(s + 1, j, z))
          }
        }
    */

    b(0)(n - 1)("SBARQ")
    //reconstructTree(0, n - 1, "SBARQ")
  }

  def parseTrees(from: String, to: String) {
    def splitLine(line: String): List[String] = {
      val arr = line.split(" ").map(word => if (word.contains("\\")) word.replaceAllLiterally("\\", "\\\\") else word).toList
      //println(arr)
      arr
    }

    printToFile(to) {
      writer => for {
        line <- Source.fromFile(from).getLines().map(splitLine)
      } writer.println(cky(line))
    }
  }
}
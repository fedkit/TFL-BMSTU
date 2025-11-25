import scala.io.Source
import scala.util.Random
import scala.collection.mutable

case class Rule(left: String, right: String)

class SRS(path: String) {
  private def loadSRS(path: String): List[Rule] = {
    val f = Source.fromFile(path)
    val lines = f.getLines().toList
    f.close()
    lines.map { line =>
      val rule = line.split("→")
      Rule(rule(0), rule(1))
    }
  }

  private def extractAlphabet(rules: List[Rule]): Set[Char] = {
    var letters = Set.empty[Char]
    for (i <- rules) {
      for (j <- i.left) {
        if (j != ' ')
          letters += j
      }
      for (j <- i.right) {
        if (j != ' ')
          letters += j
      }
    }
    letters
  }

  val srs: List[Rule] = loadSRS(path)
  val alphabet: Set[Char] = extractAlphabet(srs)
  val lenSRS: Int = srs.size
}

object FuzzingTest {
  val rand: Random = new Random(42)

  def generateWay(startWord: String, srs: SRS, minSteps: Int, maxSteps: Int): String = {
    var currentWord = startWord
    val countSteps = minSteps + rand.nextInt(maxSteps - minSteps + 1)

    for (_ <- 0 to countSteps) {
      val acceptableRules = mutable.ListBuffer[Rule]()
      for (rule <- srs.srs) {
        if (currentWord.contains(rule.left)) {
          acceptableRules += rule
        }
      }

      if (acceptableRules.nonEmpty) {
        val rule = acceptableRules(rand.nextInt(acceptableRules.size))
        val positionsIndex = mutable.ListBuffer[Int]()
        var i = 0
        while (i <= currentWord.length - rule.left.length) {
          if (currentWord.startsWith(rule.left, i)) {
            positionsIndex += i
          }
          i += 1
        }

        if (positionsIndex.nonEmpty) {
          val position = positionsIndex(rand.nextInt(positionsIndex.size))
          currentWord =
            currentWord.substring(0, position) + rule.right + currentWord.substring(position + rule.left.length)
        }
      }
    }
    currentWord
  }

  def checkSRS(startWord: String, endWord: String, srs: SRS, maxDepth: Int): Boolean = {
    def bfs(queue: mutable.Queue[(String, Int)], visited: mutable.Set[String]): Boolean = {
      if (queue.isEmpty) {
        false
      } else {
        val element = queue.dequeue()
        val currentWord = element._1
        val depth = element._2

        if (currentWord == endWord) {
          true
        } else if (depth < maxDepth) {
          val nextStates = mutable.ListBuffer[String]()
          for (rule <- srs.srs) {
            var index = 0
            while (index <= currentWord.length - rule.left.length) {
              if (currentWord.startsWith(rule.left, index)) {
                val nextState =
                  currentWord.substring(0, index) + rule.right + currentWord.substring(index + rule.left.length)
                nextStates += nextState
              }
              index += 1
            }
          }

          nextStates.foreach { next =>
            if (!visited.contains(next)) {
              visited.add(next)
              queue.enqueue((next, depth + 1))
            }
          }
          bfs(queue, visited)
        } else {
          bfs(queue, visited)
        }
      }
    }

    val queue = mutable.Queue[(String, Int)]()
    val visited = mutable.Set[String]()
    queue.enqueue((startWord, 0))
    visited.add(startWord)
    bfs(queue, visited)
  }

  def generateRandomWord(alphabet: Set[Char], minLen: Int, maxLen: Int): String = {
    val alphabetSeq = alphabet.toIndexedSeq
    val wordLen = minLen + rand.nextInt(maxLen - minLen + 1)

    val randomWord = (1 to wordLen).map { i =>
      alphabetSeq(rand.nextInt(alphabetSeq.length))
    }.mkString

    randomWord
  }

  def main(args: Array[String]): Unit = {
    val srs1 = new SRS("./test/src/SRST.txt")
    val srs2 = new SRS("./test/src/SRST'.txt")
    var countMatches: Int = 0

    for (i <- 1 to 1000000) {
      val startWord: String = generateRandomWord(srs1.alphabet, 50, 100)
      val endWord: String = generateWay(startWord, srs1, 10, 49)
      val wayToAnotherSRS: Boolean = checkSRS(startWord, endWord, srs2, 100)
      if (!wayToAnotherSRS) {
        println(startWord + " " + endWord + " \uD83D\uDC4E\uD83C\uDFFB")
      } else {
        countMatches += 1
      }
    }
    println(countMatches + " удачных действий из 1000000")
  }
}
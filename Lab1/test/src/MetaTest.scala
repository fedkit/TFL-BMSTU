import scala.util.Random
import scala.collection.mutable

object MetaTest {
  val rand: Random = new Random(0)

  def checkInv1(before: String, after: String): Boolean = {
    before.count(i => i == 'b') >= after.count(i => i == 'b')
  }

  def checkInv2(before: String, after: String): Boolean = {
    after.length <= before.length
  }

  def checkInv3(before: String, after: String): Boolean = {
    if (before.length == after.length)
      before > after
    else
      before.length > after.length
  }

  def checkInv4(before: String, after: String): Boolean = {
    def sumB(s: String): Int = {
      var sum = 0
      for (i <- 0 until s.length) {
        if (s(i) == 'b') {
          sum += i
        }
      }
      sum
    }
    sumB(after) <= sumB(before)
  }

  def checkInv5(w: String, srs: SRS): Boolean = {
    val applicableRules = mutable.ListBuffer[Rule]()

    for (rule <- srs.srs) {
      var i = 0
      while (i <= w.length - rule.left.length) {
        if (w.startsWith(rule.left, i)) {
          applicableRules += rule
          i = w.length
        } else {
          i += 1
        }
      }
    }

    if (applicableRules.isEmpty) {
      true
    } else {
      for (rule <- applicableRules) {
        var i = 0
        while (i <= w.length - rule.left.length) {
          if (w.startsWith(rule.left, i)) {
            val newWord = w.substring(0, i) + rule.right + w.substring(i + rule.left.length)
            val countAC = newWord.count(j => j == 'a' || j == 'c')
            if (countAC < 1) {
              return false
            }
            i = w.length
          } else {
            i += 1
          }
        }
      }
      true
    }
  }

  def checkInv6(before: String, after: String): Boolean = {
    if (before.count(i => i == 'b') == 0)
      after.count(i => i == 'b') == 0
    else
      true
  }

  def checkInv7(before: String, after: String): Boolean = {
    def f(s: String): Int =
      s.map {
        case 'a' | 'b' => 1
        case 'c'       => 2
      }.sum
    f(after) <= f(before)
  }

  def checkInv8(before: String, after: String): Boolean = {
    val aBefore = before.count(i => i == 'a')
    val bBefore = before.count(i => i == 'b')
    val cBefore = before.count(i => i == 'c')
    val aAfter = after.count(i => i == 'a')
    val bAfter = after.count(i => i == 'b')
    val cAfter = after.count(i => i == 'c')

    val beforeVal = BigInt(2).pow(aBefore + bBefore ) * BigInt(3).pow(cBefore)
    val afterVal = BigInt(2).pow(aAfter + bAfter) * BigInt(3).pow(cAfter)
    beforeVal >= afterVal
  }

  def checkInv9(w: String, srs: SRS): Boolean = {
    val aMatrix = Array(Array(1, 1), Array(1, 1))
    val bMatrix = Array(Array(1, 2), Array(2, 1))
    val cMatrix = Array(Array(2, 1), Array(1, 2))

    def getMatrix(c: Char): Array[Array[Int]] = c match {
      case 'a' => aMatrix
      case 'b' => bMatrix
      case 'c' => cMatrix
      case _ => Array(Array(0, 0), Array(0, 0))
    }

    def multiplyMatrices(A: Array[Array[Int]], B: Array[Array[Int]]): Array[Array[Int]] = {
      Array(
        Array(A(0)(0) * B(0)(0) + A(0)(1) * B(1)(0), A(0)(0) * B(0)(1) + A(0)(1) * B(1)(1)),
        Array(A(1)(0) * B(0)(0) + A(1)(1) * B(1)(0), A(1)(0) * B(0)(1) + A(1)(1) * B(1)(1))
      )
    }

    def stringToMatrix(s: String): Array[Array[Int]] = {
      if (s.isEmpty) Array(Array(1, 0), Array(0, 1))
      else {
        var result = getMatrix(s(0))
        for (i <- 1 until s.length) {
          result = multiplyMatrices(result, getMatrix(s(i)))
        }
        result
      }
    }

    for (rule <- srs.srs) {
      val L = stringToMatrix(rule.left)
      val R = stringToMatrix(rule.right)

      val L_minus_R = Array(
        Array(L(0)(0) - R(0)(0), L(0)(1) - R(0)(1)),
        Array(L(1)(0) - R(1)(0), L(1)(1) - R(1)(1))
      )

      for (i <- 0 until 2; j <- 0 until 2) {
        if (L_minus_R(i)(j) < 0) {
          return false
        }
      }
    }

    true
  }

  def generateRandomWord(alphabet: Set[Char], minLen: Int, maxLen: Int): String = {
    val alphabetSeq = alphabet.toIndexedSeq
    val wordLen = minLen + rand.nextInt(maxLen - minLen + 1)

    val randomWord = (1 to wordLen).map { i =>
      alphabetSeq(rand.nextInt(alphabetSeq.length))
    }.mkString

    randomWord
  }

  def checkInvs(startWord: String, minSteps: Int, maxSteps: Int, srs: SRS): Boolean = {
    var currentWord = startWord
    val steps = minSteps + rand.nextInt(maxSteps - minSteps + 1)

    if (!checkInv5(currentWord, srs))
      return false
    if (!checkInv9(currentWord, srs))
      return false

    for (i <- 0 until steps) {
      val acceptableRules = mutable.ListBuffer[Rule]()

      for (rule <- srs.srs) {
        var j = 0
        while (j <= currentWord.length - rule.left.length) {
          if (currentWord.startsWith(rule.left, j)) {
            acceptableRules += rule
            j = currentWord.length
          } else {
            j += 1
          }
        }
      }

      if (acceptableRules.nonEmpty) {
        val rule = acceptableRules(rand.nextInt(acceptableRules.size))

        val positions = mutable.ListBuffer[Int]()
        var j = 0
        while (j <= currentWord.length - rule.left.length) {
          if (currentWord.startsWith(rule.left, j)) {
            positions += j
          }
          j += 1
        }

        if (positions.nonEmpty) {
          val position = positions(rand.nextInt(positions.size))
          val oldWord = currentWord
          currentWord = currentWord.substring(0, position) + rule.right +
            currentWord.substring(position + rule.left.length)

          if (
              !checkInv1(oldWord, currentWord) ||
              !checkInv2(oldWord, currentWord) ||
              !checkInv3(oldWord, currentWord) ||
              !checkInv4(oldWord, currentWord) ||
              !checkInv6(oldWord, currentWord) ||
              !checkInv7(oldWord, currentWord) ||
              !checkInv8(oldWord, currentWord)
          )
            return false
        }
      } else {
        return true
      }
    }
    true
  }

  def main(args: Array[String]): Unit = {
    val srs2 = new SRS("./test/src/SRST'.txt")
    var countMatches: Int = 0
    for (i <- 1 to 100000) {
      val startWord: String = generateRandomWord(srs2.alphabet, 50, 100)
      val invWay: Boolean = checkInvs(startWord, 25, 49, srs2)
      if (!invWay) {
        println(startWord + " \uD83D\uDC4E\uD83C\uDFFB")
      } else {
        countMatches += 1
      }
    }
    println(countMatches + " удачных действий из 100000")
  }
}
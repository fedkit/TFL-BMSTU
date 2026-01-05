import scala.util.Random

object Main {
  val regex = "^(aa*ab|bbabb|abb*abab)*baba(a|b)(a|b)((aa)*ab|babb*aabab)*$".r

  val DFA: Map[Int, Map[Char, Int]] = Map(
    0  -> Map('b' -> 1, 'a' -> 5),
    1  -> Map('b' -> 2, 'a' -> 10),
    2  -> Map('a' -> 3, 'b' -> 999),
    3  -> Map('b' -> 4, 'a' -> 999),
    4  -> Map('b' -> 0, 'a' -> 999),
    5  -> Map('b' -> 6, 'a' -> 9),
    6  -> Map('b' -> 6, 'a' -> 7),
    7  -> Map('b' -> 8, 'a' -> 999),
    8  -> Map('a' -> 4, 'b' -> 999),
    9  -> Map('a' -> 9, 'b' -> 0),
    10 -> Map('b' -> 11, 'a' -> 999),
    11 -> Map('a' -> 12, 'b' -> 999),
    12 -> Map('a' -> 13, 'b' -> 13),
    13 -> Map('a' -> 14, 'b' -> 14),
    14 -> Map('a' -> 22, 'b' -> 15),
    15 -> Map('a' -> 16, 'b' -> 999),
    16 -> Map('b' -> 17, 'a' -> 999),
    17 -> Map('a' -> 18, 'b' -> 17),
    18 -> Map('a' -> 19, 'b' -> 999),
    19 -> Map('b' -> 20, 'a' -> 999),
    20 -> Map('a' -> 21, 'b' -> 999),
    21 -> Map('b' -> 14, 'a' -> 999),
    22 -> Map('a' -> 23, 'b' -> 14),
    23 -> Map('a' -> 22, 'b' -> 999),
    999 -> Map('a' -> 999, 'b' -> 999)
  )

  val NFA: Map[Int, Map[Char, Set[Int]]] = Map(
    0  -> Map('a' -> Set(1, 6), 'b' -> Set(8)),
    1  -> Map('b' -> Set(2), 'a' -> Set(1000)),
    2  -> Map('b' -> Set(2), 'a' -> Set(3)),
    3  -> Map('b' -> Set(4), 'a' -> Set(1000)),
    4  -> Map('a' -> Set(5), 'b' -> Set(1000)),
    5  -> Map('b' -> Set(0), 'a' -> Set(1000)),
    6  -> Map('a' -> Set(7), 'b' -> Set(1000)),
    7  -> Map('a' -> Set(7), 'b' -> Set(0)),
    8  -> Map('b' -> Set(9), 'a' -> Set(12)),
    9  -> Map('a' -> Set(10), 'b' -> Set(1000)),
    10 -> Map('b' -> Set(11), 'a' -> Set(1000)),
    11 -> Map('b' -> Set(0), 'a' -> Set(1000)),
    12 -> Map('b' -> Set(13), 'a' -> Set(1000)),
    13 -> Map('a' -> Set(14), 'b' -> Set(1000)),
    14 -> Map('a' -> Set(15), 'b' -> Set(15)),
    15 -> Map('a' -> Set(16), 'b' -> Set(16)),
    16 -> Map('a' -> Set(17), 'b' -> Set(19)),
    17 -> Map('a' -> Set(18), 'b' -> Set(16)),
    18 -> Map('a' -> Set(17), 'b' -> Set(1000)),
    19 -> Map('a' -> Set(20), 'b' -> Set(1000)),
    20 -> Map('b' -> Set(21), 'a' -> Set(1000)),
    21 -> Map('b' -> Set(21), 'a' -> Set(22)),
    22 -> Map('a' -> Set(23), 'b' -> Set(1000)),
    23 -> Map('b' -> Set(24), 'a' -> Set(1000)),
    24 -> Map('a' -> Set(25), 'b' -> Set(1000)),
    25 -> Map('b' -> Set(16), 'a' -> Set(1000)),
    1000 -> Map('a' -> Set(1000), 'b' -> Set(1000))
  )

  val AFA_SubNFA: Map[Int, Map[Char, Set[Int]]] = Map(
    -1 -> Map('a' -> Set(-1), 'b' -> Set(-2)),
    -2 -> Map('a' -> Set(-3), 'b' -> Set(-2)),
    -3 -> Map('a' -> Set(-1), 'b' -> Set(-4)),
    -4 -> Map('a' -> Set(-5), 'b' -> Set(-2)),
    -5 -> Map('a' -> Set(-5), 'b' -> Set(-5)),
    -999 -> Map('a' -> Set(-999), 'b' -> Set(-999))
  )

  val finalStatesDFA: Set[Int] = Set(14)
  val finalStatesNFA: Set[Int] = Set(16)
  val finalStatesAFA_SubNFA: Set[Int] = Set(-5)

  def randomWord(minLen: Int, maxLen: Int): String = {
    val alphabet = Array('a', 'b')
    val length = Random.between(minLen, maxLen + 1)
    val sb = new StringBuilder
    for (_ <- 0 until length) sb.append(alphabet(Random.nextInt(alphabet.length)))
    sb.toString()
  }

  def check_regex(word: String): Boolean = regex.matches(word)

  def check_DFA(word: String): Boolean = {
    var state = 0
    for (c <- word) {
      DFA.get(state) match {
        case Some(transitions) =>
          state = transitions.getOrElse(c, 999)
        case None => return false
      }
    }
    finalStatesDFA.contains(state)
  }

  def check_NFA(word: String): Boolean = {
    var currentStates: Set[Int] = Set(0)
    for (c <- word) {
      val nextStates = currentStates.flatMap(state =>
        NFA.get(state).flatMap(_.get(c)).getOrElse(Set(1000))
      )
      currentStates = nextStates
    }
    currentStates.exists(finalStatesNFA.contains)
  }

  def check_SubNFA(word: String): Boolean = {
    var currentStates: Set[Int] = Set(-1)
    for (c <- word) {
      val nextStates = currentStates.flatMap(state =>
        AFA_SubNFA.get(state).flatMap(_.get(c)).getOrElse(Set(-999))
      )
      currentStates = nextStates
    }
    currentStates.exists(finalStatesAFA_SubNFA.contains)
  }

  def check_AFA(word: String): Boolean = {
    check_DFA(word) && check_SubNFA(word)
  }

  def check_random_word(): Unit = {
    for (_ <- 1 to 100000) {
      val word = randomWord(4, 35)
      if (!((check_regex(word) && check_DFA(word) && check_NFA(word) && check_AFA(word)) ||
        (!check_regex(word) && !check_DFA(word) && !check_NFA(word) && !check_AFA(word)))) {
        println("Проблема со словом - " + word)
      }
    }
  }

  def randomRegexWord(): String = {
    val rand = new Random()

    def randomBlock1(): String = {
      val options = Seq(
        "aa" + "a" * rand.nextInt(5) + "b",
        "bbabb",
        "ab" + "b" * rand.nextInt(5) + "abab"
      )
      options(rand.nextInt(options.length))
    }

    def randomBlock4(): String = {
      val options = Seq(
        "aa" * rand.nextInt(5) + "ab",
        "bab" + "b" * rand.nextInt(5) + "aabab"
      )
      options(rand.nextInt(options.length))
    }

    val sb = new StringBuilder

    for (_ <- 0 until rand.nextInt(5))
      sb.append(randomBlock1())
    sb.append("baba")
    sb.append(if (rand.nextBoolean()) 'a' else 'b')
    sb.append(if (rand.nextBoolean()) 'a' else 'b')
    for (_ <- 0 until rand.nextInt(5))
      sb.append(randomBlock4())
    sb.toString()
  }

  def check_regex_word(): Unit = {
    for (_ <- 1 to 100000) {
      val word = randomRegexWord()
      if (!(check_DFA(word) && check_NFA(word) && check_AFA(word))) {
        println("Проблема со словом - " + word, check_DFA(word), check_NFA(word), check_AFA(word))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    Random.setSeed(42)
    check_random_word()
    println()
    check_regex_word()
  }
}

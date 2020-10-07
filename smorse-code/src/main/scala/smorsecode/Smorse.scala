package smorsecode

object Smorse {

  private val morseCodeValues: Map[Char, String] = Map(
    'a' -> ".-", 'b' -> "-...", 'c' -> "-.-.", 'd' -> "-..", 'e' -> ".", 'f' -> "..-.",  'g' -> "--.", 'h' -> "....",
    'i' -> "..", 'j' -> ".---",  'k' -> "-.-", 'l' -> ".-..", 'm' -> "--", 'n' -> "-.", 'o' -> "---", 'p' -> ".--.",
    'q' -> "--.-", 'r' -> ".-.", 's' -> "...", 't' -> "-", 'u' -> "..-", 'v' -> "...-", 'w' -> ".--", 'x' -> "-..-",
    'y' -> "-.--", 'z' -> "--.."
  )

  def smorse(word: String): String = {
    word.map(morseCodeValues(_)).mkString
  }

}

object WordFilter {

  def filterWords(words: Seq[String], condition: String => Boolean): Seq[String] ={
    words.filter(condition(_))
  }

  def exactWordFilter(word: String)(current: String): Boolean = {
    current != word
  }

  def wordLengthFilter(length: Int)(current: String): Boolean = {
    current.length == length
  }

  def characterAtIndexFilter(character: Char, index: Int)(current: String): Boolean = {
    current.charAt(index) == character
  }

  def keepWordsWithGuessCharacterAtIndexes(words: Seq[String], indexes: Seq[Int], guess: Char): Seq[String] = {
    if(indexes.isEmpty){
      words
    }
    else{
      keepWordsWithGuessCharacterAtIndexes(filterWords(words, characterAtIndexFilter(guess, indexes.head)), indexes.drop(1), guess)
    }
  }

  def doesNotContainCharacterFilter(character: Char)(current: String): Boolean = {
    !current.contains(character)
  }

}

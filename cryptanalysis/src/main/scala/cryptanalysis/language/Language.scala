package cryptanalysis.language

trait Language {

  def alphabet: String

  def letterProbabilities: Seq[Double]

  def normalizeString(s: String): String

  def normalizeStringWithoutSpaces(s: String): String

}

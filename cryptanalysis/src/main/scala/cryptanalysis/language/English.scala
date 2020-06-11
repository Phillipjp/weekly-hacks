package cryptanalysis.language

object English extends Language {

  override def alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  // ordered from A-Z
  override def letterProbabilities: Seq[Double] = Seq(8.12, 1.49, 2.71, 4.32, 12.02, 2.3, 2.03, 5.92, 7.31, 0.1, 0.69, 3.98, 2.61,
    6.95, 7.68, 1.82, 0.11, 6.02, 6.28, 9.1, 2.88, 1.11, 2.09, 0.17, 2.11, 0.07)

  def normalizeString(s: String): String = {
    s.replaceAll("""[\p{Punct}]|£|\n""", "").filterNot(_.isDigit).toUpperCase
  }

  def normalizeStringWithoutSpaces(s: String): String = {
    s.replaceAll("""[\p{Punct}]|£|\n""", "").filterNot(_.isDigit).replaceAll(" ", "").toUpperCase
  }
}

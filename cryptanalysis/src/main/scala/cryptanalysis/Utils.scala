package cryptanalysis

object Utils {

  def normalizeString(s: String): String = {
    s.replaceAll("""[\p{Punct}]|£""", "").toUpperCase
  }

}

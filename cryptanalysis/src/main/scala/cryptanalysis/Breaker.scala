package cryptanalysis

import cryptanalysis.language.Language

trait Breaker[K] {

  def break(cipherText: String, language: Language): Seq[(K, String)]

}

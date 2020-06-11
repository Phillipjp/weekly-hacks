package cryptanalysis

import cryptanalysis.language.Language

trait Cipher[K] {

  def encrypt(plaintext: String, key: K, language: Language): String

  def decrypt(cipherText: String, key: K, language: Language): String

}

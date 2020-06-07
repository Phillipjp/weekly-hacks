package cryptanalysis

class CaesarCypherEncrypter(shift: Int) {

 private val caesarMapping = getCaesarMapping()

   private [cryptanalysis] def getCaesarMapping(): Map[Char, Char] = {

     val standardisedShift = if(shift < 0){
       26 + shift
     }
     else{
       shift
     }

    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

     val firstHalf = alphabet.substring(0, alphabet.length - standardisedShift).map{letter => (letter + standardisedShift).toChar}
     val secondHalf = alphabet.take(standardisedShift)

     val encoded = firstHalf + secondHalf

     alphabet.zip(encoded).toMap + (' ' -> ' ')

  }
  def encrypt(plainText: String): String = {
    val normalizedPlainText = Utils.normalizeString(plainText)
    val encryptredText = normalizedPlainText.map(letter => caesarMapping(letter))
    encryptredText
  }

}

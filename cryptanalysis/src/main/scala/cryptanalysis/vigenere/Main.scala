package cryptanalysis.vigenere

import cryptanalysis.language.English

object Main {

  def main(args: Array[String]): Unit = {

//    val plainText = "the quick brown fox jumps over the lazy dog"
    val plainText = "Be patient till the last. Romans, countrymen, and lovers! hear me for my cause, and be silent, that you may hear: believe me for mine honour, and have respect to mine honour, that you may believe: censure me in your wisdom, and awake your senses, that you may the better judge. If there be any in this assembly, any dear friend of Caesar's, to him I say, that Brutus' love to Caesar was no less than his. If then that friend demand why Brutus rose against Caesar, this is my answer: Not that I loved Caesar less, but that I loved Rome more. Had you rather Caesar were living and die all slaves, than that Caesar were dead, to live all free men? As Caesar loved me, I weep for him; as he was fortunate, I rejoice at it; as he was valiant, I honour him: but, as he was ambitious, I slew him. There is tears for his love; joy for his fortune; honour for his valour; and death for his ambition. Who is here so base that would be a bondman? If any, speak; for him have I offended. Who is here so rude that would not be a Roman? If any, speak; for him have I offended. Who is here so vile that will not love his country? If any, speak; for him have I offended. I pause for a reply. Then none have I offended. I have done no more to Caesar than you shall do to Brutus. The question of his death is enrolled in the Capitol; his glory not extenuated, wherein he was worthy, nor his offences enforced, for which he suffered death. Here comes his body, mourned by Mark Antony: who, though he had no hand in his death, shall receive the benefit of his dying, a place in the commonwealth; as which of you shall not? With this I depart,that, as I slew my best lover for the good of Rome, I have the same dagger for myself, when it shall please my country to need my death."
    val cipherText = VigenereCipher.encrypt(plainText, "brutus", English)

    println(s"PLAIN TEXT:  ${English.normalizeString(plainText)}")

    println(s"CIPHER TEXT: $cipherText")

    println("============================================================")
    val top5 = VigenereBreaker.break(cipherText, English)
    top5.zipWithIndex.foreach(x => println(s"BREAK ATTEMPT ${x._2+1}: ${x._1}"))
    println("============================================================")
    println(s"DECRYPT: ${VigenereCipher.decrypt(cipherText, "brutus", English)}")

  }
}

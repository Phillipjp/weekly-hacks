package talkingclock

object MinuteToWordMap {
  private val tensMinuteToWord: Map[String, String] = Map(
    "0" -> "oh",
    "2" -> "twenty",
    "3" -> "thirty",
    "4" -> "forty",
    "5" -> "fifty"
  )

  private val unitMinuteToWord: Map[String, String] = Map(
    "0" -> "",
    "1" -> " one",
    "2" -> " two",
    "3" -> " three",
    "4" -> " four",
    "5" -> " five",
    "6" -> " six",
    "7" -> " seven",
    "8" -> " eight",
    "9" -> " nine"
  )

  private val teenMinuteToWord: Map[String, String] = Map(
    "10" -> "ten",
    "11" -> "eleven",
    "12" -> "twelve",
    "13" -> "thirteen",
    "14" -> "fourteen",
    "15" -> "fifteen",
    "16" -> "sixteen",
    "17" -> "seventeen",
    "18" -> "eighteen",
    "19" -> "nineteen",
  )

  def get(minutesAsNumber: String): Option[String] = {
    val tensMinute = minutesAsNumber.head.toString
    val unitMinute = minutesAsNumber.last.toString

    if(tensMinute == "1")
      teenMinuteToWord.get(minutesAsNumber)
    else if (minutesAsNumber == "00")
      Option("")
    else {
      (tensMinuteToWord.get(tensMinute), unitMinuteToWord.get(unitMinute)) match {
        case (Some(tm), Some(um)) => Option(s"$tm$um")
        case _ => None
      }
    }
  }

}

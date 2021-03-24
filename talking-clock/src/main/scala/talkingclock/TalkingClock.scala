package talkingclock

object TalkingClock {

  private val hourToWord: Map[String, String] = Map(
    "00" -> "twelve", "01" -> "one", "02" -> "two", "03" -> "three", "04" -> "four", "05" -> "five", "06" -> "six", "07" -> "seven", "08" -> "eight", "09" -> "nine", "10" -> "ten", "11" -> "eleven",
    "12" -> "twelve", "13" -> "one", "14" -> "two", "15" -> "three", "16" -> "four", "17" -> "five", "18" -> "six", "19" -> "seven", "20" -> "eight", "21" -> "nine", "22" -> "ten", "23" -> "eleven"
  )

  def timeToSentence(timeString: TimeString): String = {

    val splitTime = timeString.time.split(":")

    val minutes = MinuteToWord.get(splitTime.last)

    val timeIdentifier = getTimeIdentifier(splitTime.head.toInt)

    val list = List("It's", hourToWord(splitTime.head), minutes.getOrElse(""), timeIdentifier.getOrElse("")).filter(_ != "")

    list.mkString(" ")
  }

  def timeToSentence(time: String): Option[String] = {

    val timePattern = "\\d\\d:\\d\\d".r

    if(timePattern.pattern.matcher(time).matches() &&
      time.split(":").head.toInt < 24 &&
      time.split(":").head.toInt >= 0 &&
      time.split(":").last.toInt < 60 &&
      time.split(":").last.toInt >= 0
    ) {

      val splitTime = time.split(":")

      val minutes = MinuteToWord.get(splitTime.last)

      val timeIdentifier = getTimeIdentifier(splitTime.head.toInt)

      val list = List("It's", hourToWord(splitTime.head), minutes.getOrElse(""), timeIdentifier.getOrElse("")).filter(_ != "")

      Option(list.mkString(" "))
    }
    else{
      None
    }
  }

  private[talkingclock] def getTimeIdentifier(hoursAsNumber: Int): Option[String] = {
    if (hoursAsNumber >= 0 && hoursAsNumber < 12)
      Option("am")
    else if (hoursAsNumber > 11 && hoursAsNumber < 24)
      Option("pm")
    else
      None
  }

}

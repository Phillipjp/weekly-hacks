package talkingclock

case class TimeString(time: String){
  private val digitPattern = "\\d".r
  require(time.length == 5)
  require(time.charAt(2) == ':')
  require(time.filter(_ != ':').forall(c => digitPattern.pattern.matcher(c.toString).matches()))
  require {
    val splitTime = time.split(":")
    val hour = splitTime.head.toInt
    val minute = splitTime.last.toInt

    hour >= 0 && hour < 24 && minute >= 0 && minute < 60
  }
}

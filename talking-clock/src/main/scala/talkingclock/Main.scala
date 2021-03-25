package talkingclock

import talkingclock.TalkingClock.timeToSentence
import java.time.LocalTime
import java.time.format.DateTimeFormatter
object Main {

  def main(args: Array[String]): Unit = {

//    println(timeToSentence(TimeString("12:05")))
//    println(timeToSentence(TimeString("00:00")))
//    println(timeToSentence(TimeString("01:30")))
//    println(timeToSentence(TimeString("12:05")))
//    println(timeToSentence(TimeString("14:01")))
//    println(timeToSentence(TimeString("20:29")))
//    println(timeToSentence(TimeString("21:00")))
//
//    val sentence = timeToSentence(TimeString("21:00"))
//    Speaker.speak(sentence)

    while(true) {

      val localTime = LocalTime.now()
      val formatter = DateTimeFormatter.ofPattern("HH:mm")

      val formattedTime = localTime.format(formatter)

      val sentence = timeToSentence(formattedTime).getOrElse("Invalid Time")
      println(sentence)
      val minute = localTime.getMinute
      if(minute == 0 || minute%15 == 0 ) {
        Speaker.speak(sentence.dropRight(1) + "-m")
      }

      Thread.sleep(60*1000)

    }


  }



}

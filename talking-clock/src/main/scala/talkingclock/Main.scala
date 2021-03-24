package talkingclock

import talkingclock.TalkingClock.timeToSentence

object Main {

  def main(args: Array[String]): Unit = {

    println(timeToSentence(TimeString("12:05")))
    println(timeToSentence(TimeString("00:00")))
    println(timeToSentence(TimeString("01:30")))
    println(timeToSentence(TimeString("12:05")))
    println(timeToSentence(TimeString("14:01")))
    println(timeToSentence(TimeString("20:29")))
    println(timeToSentence(TimeString("21:00")))

    val sentence = timeToSentence(TimeString("21:00"))
    Speaker.speak(sentence)

  }



}

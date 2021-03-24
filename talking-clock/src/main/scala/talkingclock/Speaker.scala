package talkingclock
import sys.process._
object Speaker {

  def speak(sentence: String): Unit = {
    s"say $sentence" !
  }

}

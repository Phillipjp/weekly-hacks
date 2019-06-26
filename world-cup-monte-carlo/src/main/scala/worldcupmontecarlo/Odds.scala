package worldcupmontecarlo

case class Odds(numerator: Int, denominator: Int) {

  private def calculateGCD(a: Int, b: Int): Int = {
    if(a == 0)
      b
    else if(b == 0)
      a
    else
      calculateGCD(b, a % b)
  }

  private def normalise(): String = {

    val gcd = calculateGCD(numerator , denominator)
    val normalisedNumerator = numerator/gcd
    val normalisedDenominator = denominator/gcd

    normalisedNumerator + "/" + normalisedDenominator
  }

  def decimal(): Double = {
    numerator.toDouble/denominator.toDouble
  }

  override def toString: String = {
    normalise()
  }
}

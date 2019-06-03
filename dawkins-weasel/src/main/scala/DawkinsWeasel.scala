import scala.util.Random

trait DawkinsWeasel {

  def dawkinsWeasel(target: Seq[Char], genePool: String, geneSequenceLength: Int, rand: Random): Seq[Char]

  def printIteration(i: Int, sequence: Seq[Char]): Unit ={
    println()
    print(s"ITERATION $i: ")
    sequence.foreach(print)
  }

  def generateRandomGeneSequence(genePool: String, geneSequenceLength: Int, rand: Random): Seq[Char] ={
    Stream.continually(randomGene(genePool, rand)).take(geneSequenceLength)
  }

  def reproduceGeneSequence(x :Int, sequence: Seq[Char]): Seq[Seq[Char]] = {
    Stream.continually(sequence).take(x)
  }

  def mutateGene(gene: Char, genePool: String, rand: Random): Seq[Char]

  def randomGene(genePool: String, rand: Random): Char

  def mutateGeneSequence(sequence: Seq[Char], genePool: String, rand: Random): Seq[Char]

  def scoreSequence(sequence: Seq[Char], target: Seq[Char]): Int

}

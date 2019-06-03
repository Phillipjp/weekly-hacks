import org.apache.commons.text.similarity.LevenshteinDistance

import scala.util.Random

class LevenshteinDawkinsWeasel extends DawkinsWeasel {

  val TARGET: Seq[Char] = "METHINKS IT IS LIKE A WEASEL".toSeq
  val GENEPOOL = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
  val GENE_SEQUENCE_LENGTH: Int = TARGET.length

  def dawkinsWeasel(target: Seq[Char], genePool: String, geneSequenceLength: Int, rand: Random): Seq[Char] = {
    val initialSequence = generateRandomGeneSequence(genePool, geneSequenceLength, rand)

    def findTargetSequence(sequence: Seq[Char], target: Seq[Char], genePool: String, i: Int, rand: Random): Seq[Char] = {
      if (sequence == target){
        printIteration(i, sequence)
        sequence
      }
      else {
        val mostSimilarSequence = reproduceGeneSequence(100, sequence)
          .map(mutateGeneSequence(_, genePool, rand))
          .minBy(scoreSequence(_, target))
        printIteration(i, mostSimilarSequence)

        findTargetSequence(mostSimilarSequence, target, genePool, i + 1, rand)

      }
    }

    findTargetSequence(initialSequence, target, genePool, 1, rand)
  }

  def mutateGene(gene: Char, genePool: String, rand: Random): Seq[Char] = {
    val mutateProb = rand.nextInt(100)
    if(mutateProb % 20 == 0){
      val changeGeneProp = rand.nextInt(100)
      if(changeGeneProp % 2 == 0){
        changeGene(gene, genePool, rand)
      }
      else {
        val addGeneProp = rand.nextInt(100)
        if(addGeneProp % 2 == 0){
          addGene(gene, genePool, rand)
        }
        else{
          Seq()
        }
      }
    }
    else{
      gene.toString
    }
  }

  def changeGene(gene: Char, genePool: String, rand: Random): Seq[Char] = {
      Seq(randomGene(genePool, rand))
  }

  def addGene(gene: Char, genePool: String, rand: Random): Seq[Char] = {
    Seq(gene, randomGene(genePool, rand))
  }

  def randomGene(genePool: String, rand: Random): Char = {
    genePool.charAt(rand.nextInt(genePool.length))
  }

  def mutateGeneSequence(sequence: Seq[Char], genePool: String, rand: Random): Seq[Char] = {
    sequence.map(gene => mutateGene(gene, genePool, rand)).filter(_.nonEmpty).flatten
  }

  def scoreSequence(sequence: Seq[Char], target: Seq[Char]): Int = {
    new LevenshteinDistance().apply(sequence.mkString(""), target.mkString(""))
  }
}

object LevenshteinDawkinsWeasel {
  def main(args: Array[String]): Unit = {
    val weasel = new LevenshteinDawkinsWeasel()
    weasel.dawkinsWeasel(weasel.TARGET, weasel.GENEPOOL, weasel.GENE_SEQUENCE_LENGTH, new Random())

  }
}

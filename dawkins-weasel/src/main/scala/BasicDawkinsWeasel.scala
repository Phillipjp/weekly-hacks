import scala.util.Random

class BasicDawkinsWeasel extends DawkinsWeasel {

  val TARGET: Seq[Char] = "METHINKS IT IS LIKE A WEASEL".toSeq
  val GENEPOOL: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
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
          .maxBy(scoreSequence(_, target))
        printIteration(i, mostSimilarSequence)

        findTargetSequence(mostSimilarSequence, target, genePool, i + 1, rand)

      }
    }

    findTargetSequence(initialSequence, target, genePool, 1, rand)
  }

  def mutateGene(gene: Char, genePool: String, rand: Random): Seq[Char] = {
    val mutateProb = rand.nextInt(100)
    if(mutateProb % 20 == 0){
      Seq(randomGene(genePool, rand))
    }
    else{
      Seq(gene)
    }
  }

  def randomGene(genePool: String, rand: Random): Char = {
    genePool.charAt(rand.nextInt(genePool.length))
  }

  def mutateGeneSequence(sequence: Seq[Char], genePool: String, rand: Random): Seq[Char] = {
    sequence.flatMap(gene => mutateGene(gene, genePool, rand))
  }

  def scoreSequence(sequence: Seq[Char], target: Seq[Char]): Int = {
    if(sequence == target){
      sequence.length
    }
    else{
      sequence.zip(target).count(genes => genes._1 == genes._2)
    }
  }
}

object BasicDawkinsWeasel {
  def main(args: Array[String]): Unit = {
    val weasel = new BasicDawkinsWeasel()
    weasel.dawkinsWeasel(weasel.TARGET, weasel.GENEPOOL, weasel.GENE_SEQUENCE_LENGTH, new Random())

  }
}

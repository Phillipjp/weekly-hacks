import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito
import org.scalatest.mockito.MockitoSugar
import scala.util.Random

class BasicDawkinsWeaselTest extends FlatSpec with Matchers with MockitoSugar{

  val TARGET = "ABBA BBC BAC".toSeq
  val GENEPOOL = "ABC "
  val SEED = 5000

  it should "copy a sequence n times and store in a sequence" in {
    val initialSequence = Seq('A', 'B', 'C')
    val expected = Seq(initialSequence, initialSequence, initialSequence)
    val actual = new BasicDawkinsWeasel().reproduceGeneSequence(3, initialSequence)

    actual should equal(expected)
  }

  it should "Give the maximum score to a sequence identical to the target" in {
    val actual = new BasicDawkinsWeasel().scoreSequence(TARGET, TARGET)
    actual should equal(TARGET.length)
  }

  it should "Only score the genes that are in the correct position" in {
    val actual = new BasicDawkinsWeasel().scoreSequence("ABBAQWERTA B".toSeq, TARGET)
    actual should equal(4)
  }

  it should "Randomly select a gene from the gene pool" in {
    val newGene = new BasicDawkinsWeasel().randomGene(GENEPOOL, new Random(SEED))
    newGene should equal('A')
  }

  it should "Create a random sequence of genes" in {
    val geneSequence = new BasicDawkinsWeasel().generateRandomGeneSequence(GENEPOOL, 8, new Random(SEED))
    geneSequence should equal(Seq('A', 'B', ' ', 'B', 'B', 'C', 'C', 'B'))
  }

  it should "Mutate a gene, given the probability of mutating is true" in {
    val rand = mock[Random]
    Mockito.when(rand.nextInt(100)).thenReturn(20)
    Mockito.when(rand.nextInt(GENEPOOL.length)).thenReturn(1)
    val mutatedGene = new BasicDawkinsWeasel().mutateGene('A', GENEPOOL, rand)
    mutatedGene should equal(Seq('B'))
  }

  it should "Do nothing, given the probability of mutating is false" in {
    val rand = mock[Random]
    Mockito.when(rand.nextInt(100)).thenReturn(21)
    val mutatedGene = new BasicDawkinsWeasel().mutateGene('A', GENEPOOL, rand)
    mutatedGene should equal(Seq('A'))
  }

  it should "Mutate a gene sequence" in {
    val mutatedSequence = new BasicDawkinsWeasel().mutateGeneSequence("AB CD AAC BA".toSeq, GENEPOOL, new Random(SEED))
    mutatedSequence should equal("AB CD ACC BA".toSeq)
  }
}

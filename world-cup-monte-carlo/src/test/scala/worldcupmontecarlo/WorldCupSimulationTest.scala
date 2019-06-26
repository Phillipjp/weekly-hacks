package worldcupmontecarlo

import scala.util.Random
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.scalatest._

class WorldCupSimulationTest extends FlatSpec with Matchers with MockitoSugar{

  val teams = Seq(
    Team("Norway", 1915),
    Team("Australia", 2043),
    Team("England", 2043),
    Team("Cameroon", 2043),
    Team("France", 2043),
    Team("Brazil", 2043),
    Team("Spain", 2043),
    Team("USA", 2043),
    Team("Italy", 2043),
    Team("Sweden", 2043),
    Team("Netherlands", 2043),
    Team("Japan", 2043),
    Team("Germany", 2043),
    Team("Nigeria", 2043),
    Team("Sweden", 2043),
    Team("Canada", 2043)
  )


  behavior of "WorldCupSimulation"

  it should "predict the winner of a fixture" in {
    val teamA = Team("USA", 2101)
    val teamB = Team("Germany", 2072)

    val wc = new WorldCupSimulation

    val actual = wc.simulateMatch(teamA, teamB, new Random(500))

    actual should equal(teamB)
  }

  it should "simulate all matches in a round" in {
    val worldCup = new WorldCupSimulation
    val actual = worldCup.simulateRound(teams, new Random(500))

    val expected = Stream(
      Team("Australia" ,2043),
      Team("England",2043),
      Team("Brazil",2043),
      Team("USA",2043),
      Team("Sweden",2043),
      Team("Netherlands",2043),
      Team("Nigeria",2043),
      Team("Sweden",2043))

    actual should equal(expected)
  }

  it should "not recurse if there's only one team left" in {
    val mockWorldCup = mock[WorldCupSimulation]
    mockWorldCup.simulateTournament(Seq(Team("USA", 2043)), new Random(500))
    verify(mockWorldCup, never).simulateTournament(Seq(Team("USA", 2043)), new Random(500))
  }

}


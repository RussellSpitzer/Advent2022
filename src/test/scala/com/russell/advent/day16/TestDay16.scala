package com.russell.advent.day16

import com.russell.advent.AdventUtil.Position
import com.russell.advent.day16.Day16._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay16 extends AnyFlatSpec with should.Matchers {

  val Example = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                  |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                  |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                  |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                  |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                  |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                  |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                  |Valve HH has flow rate=22; tunnel leads to valve GG
                  |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                  |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin.split("\n").map(_.trim)

  "Parse Input" should "correctly parse example" in {
    val valveMap = parseInput(Example)
  }

  "Best path" should "find the highest value path in the example" in {
    val valveMap = parseInput(Example)
    val firstChoiceMap = bestPath(valveMap)
    /*
    firstChoiceMap.sortBy(s => s.map(_.score).sum).reverse
      .foreach(line => println(s"${line.map(_.score).sum} points : ${line.reverse}"))
     */
    val path = firstChoiceMap.sortBy(s => s.map(_.score).sum).last
    path.map(_.name) should be (Seq("DD", "BB", "JJ", "HH", "EE", "CC"))
    path.map(_.score).sum should be (1651)
  }

  it should "work with the input data" in {
    val valveMap = parseInput("day16.txt")
    val firstChoiceMap = bestPath(valveMap)
    /*
    firstChoiceMap.sortBy(s => s.map(_.score).sum).reverse
      .foreach(line => println(s"${line.map(_.score).sum} points : ${line.reverse}"))
     */
    val path = firstChoiceMap.sortBy(s => s.map(_.score).sum).last
    path.map(_.name) should be(Seq("TA", "QK", "JA", "VK", "ID", "DW", "EQ"))
    path.map(_.score).sum should be(2330)

  }

  "With an elephant" should "work with example" in {
    val valveMap = parseInput(Example)
    val firstChoiceMap = bestPath(valveMap, timeLeft =  26)

    val totalAmount = firstChoiceMap.size
    var i = 0
    val path = firstChoiceMap.map { myPath =>
      val visited = myPath.map(_.name).toSet
      val afterSecondRun = bestPath(valveMap, visited = visited, timeLeft = 26)
      i += 1
      if (i % (totalAmount / 1000) == 0) {
        println(s"$i / $totalAmount")
      }
      if (afterSecondRun.nonEmpty) {
        myPath ++ afterSecondRun.maxBy(_.map(_.score).sum)
      } else {
        Seq.empty
      }
    }.maxBy(_.map(_.score).sum)

    println(s"$path : ${path.map(_.score).sum}")
    path.map(_.score).sum should be (1707)
  }

  it should "work with the input data" in {
    val valveMap = parseInput("day16.txt")
    val firstChoiceMap = bestPath(valveMap, timeLeft = 26)

    val totalAmount = firstChoiceMap.size
    var i = 0
    val bestCombo = firstChoiceMap.map { myPath =>
      val visited = myPath.map(_.name).toSet
      val afterSecondRun = bestPath(valveMap, visited = visited, timeLeft = 26)
      i += 1
      if (i % (totalAmount / 1000) == 0) {
        println (s"$i / $totalAmount")
      }
      afterSecondRun.sortBy(_.map(_.score).sum).last
    }.sortBy(_.map(_.score).sum).last

    println(s"$bestCombo : ${bestCombo.map(_.score).sum}")
    bestCombo.map(_.score).sum should be(1707)
  }


}

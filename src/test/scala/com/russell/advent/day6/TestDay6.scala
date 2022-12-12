package com.russell.advent.day6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import Day6._

class TestDay6 extends AnyFlatSpec with should.Matchers {

  val Examples = Seq(
    "bvwbjplbgvbhsrlpgdmjqwftvncz", //: first marker after character 5
    "nppdvjthqldpwncqszvftbrmjlhg", //: first marker after character 6
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", // : first marker after character 10
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" //: first marker after character 11
  )

  val Examples2 = Seq(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb", // first marker after character 19
    "bvwbjplbgvbhsrlpgdmjqwftvncz", // first marker after character 23
    "nppdvjthqldpwncqszvftbrmjlhg", // first marker after character 23
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", // first marker after character 29
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" // first marker after character 26
  )

  "Find first 4 different characters" should "work in the examples" in {
    findFirstDifferentN(Examples(0)).get._2 should be (5)
    findFirstDifferentN(Examples(1)).get._2 should be (6)
    findFirstDifferentN(Examples(2)).get._2 should be (10)
    findFirstDifferentN(Examples(3)).get._2 should be (11)
  }

  it should "work in part1" in {
    val result = findFirstDifferentN(parseInput("day6.txt")).get
    result._1.distinct.length should be (4)
    println(result)
  }

  "Find first 14 different characters" should "work in the examples" in {
    findFirstDifferentN(Examples2(0), 14).get._2 should be(19)
    findFirstDifferentN(Examples2(1), 14).get._2 should be(23)
    findFirstDifferentN(Examples2(2), 14).get._2 should be(23)
    findFirstDifferentN(Examples2(3), 14).get._2 should be(29)
    findFirstDifferentN(Examples2(4), 14).get._2 should be(26)
  }

  it should "work in part2" in {
    val result = findFirstDifferentN(parseInput("day6.txt"), 14).get
    result._1.distinct.length should be(14)
    println(result)
  }

}

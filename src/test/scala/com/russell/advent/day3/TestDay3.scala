package com.russell.advent.day3

import com.russell.advent.day3.Day3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source
class TestDay3 extends AnyFlatSpec with should.Matchers {

  val Example ="""vJrwpWtwJgWrhcsFMMfFFhFp
                 |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                 |PmmdzqPrVvPwwTWBwg
                 |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                 |ttgJtRGJQctTZtZT
                 |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  "Parse Input" should "parse valid sacks" in {
    val sacks = parseInput("day3.txt")

    for (sack <- sacks) {
      sack._1.length should be (sack._2.length)
      val common = commonElement(sack._1, sack._2)
      for (element <- common) {
        element should be (common(0))
      }
    }
  }

  "Scores" should "be valid" in {
    scoreElement('a') should be (1)
    scoreElement('z') should be (26)
    scoreElement('A') should be (27)
    scoreElement('Z') should be (52)
  }

  "Part1 solution" should "be findable" in {
    val sacks = parseInput("day3.txt")
    val sum = sacks
      .map(sack => commonElement(sack._1, sack._2))
      .map(common => scoreElement(common(0)))
      .sum
    println(sum)
  }

  "Example" should "work in part2" in {
    val lines = Example.split("\n")
    allTriples(lines) should be (70)
  }

  "Part2 solution" should "be finadable" in {
    val lines = Source.fromResource("day3.txt").getLines().map(_.trim).toSeq
    println(allTriples(lines))
  }


}

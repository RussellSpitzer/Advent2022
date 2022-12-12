package com.russell.advent.day2

import com.russell.advent.day2.Day2
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay2 extends AnyFlatSpec with should.Matchers {

  "Parsing" should "correctly parse a file" in {
    val matches = Day2.parseInput("day2.txt")
    matches.size should be > 0
  }

  "Score" should "correctly judge matches" in {
    Day2.score("A", "A") should be (4)
    Day2.score("A", "B") should be (8)
    Day2.score("A", "C") should be (3)

    Day2.score("B", "A") should be(1)
    Day2.score("B", "B") should be(5)
    Day2.score("B", "C") should be(9)

    Day2.score("C", "A") should be(7)
    Day2.score("C", "B") should be(2)
    Day2.score("C", "C") should be(6)
  }

  "Part2 Strategy" should "correctly produce outcomes" in {
    Day2.Part2Strategy("A", "X") should be ("C")
    Day2.Part2Strategy("A", "Y") should be ("A")
    Day2.Part2Strategy("A", "Z") should be ("B")

    Day2.Part2Strategy("B", "X") should be("A")
    Day2.Part2Strategy("B", "Y") should be("B")
    Day2.Part2Strategy("B", "Z") should be("C")

    Day2.Part2Strategy("C", "X") should be("B")
    Day2.Part2Strategy("C", "Y") should be("C")
    Day2.Part2Strategy("C", "Z") should be("A")
  }

  "Using the default strategy guide" should "score" in {
    val score = Day2.scoreMatches(Day2.parseInput("day2.txt"), Day2.Part1Strategy)
    println(score)
  }

  "Using the outcome guide" should "score" in {
    val score = Day2.scoreMatches(Day2.parseInput("day2.txt"), Day2.Part2Strategy)
    println(score)
  }

}

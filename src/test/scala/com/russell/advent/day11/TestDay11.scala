package com.russell.advent.day11

import com.russell.advent.day11.Day11._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class TestDay11 extends AnyFlatSpec with should.Matchers {

  val Example =
    """"Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1""".stripMargin.split("\n").map(_.trim)

  "Parser" should "work with example" in {
    val example = parseInput(Example)
    example(0).items should be (Seq(79, 98))
    example(0).operation(2) should be (38)
    example(0).test(23) should be (true)
    example(0).test(24) should be (false)
    example(0).destination should be ((2,3))
  }

  "Example monkeys" should "have correct output after 1 round" in {
    val example = parseInput(Example)
    runRound(example)
    example.map(_.items) shouldBe Seq(
      Seq(20, 23, 27, 26),
      Seq(2080, 25, 167, 207, 401, 1046),
      Seq(),
      Seq())
  }

  it should "have correct output after 20 rounds" in {
    val example = parseInput(Example)
    for (i <- 1 to 20) {
      runRound(example)
    }
    example.map(_.items) shouldBe Seq(
      Seq(10, 12, 14, 26, 34),
      Seq(245, 93, 53, 199, 115),
      Seq(),
      Seq())

    example.map(_.numInspection) should be (Seq(101, 95, 7, 105))

    scoreMonkeys(example) should be (10605)
  }

  "Part1" should "parse correctly" in {
    val part1 = parseInput("day11.txt")
    part1.length should be (8)
    part1(5).items should be (Seq(79, 53))
    part1(5).operation(2) should be (3)
    part1(5).test(5) should be (false)
    part1(5).test(6) should be (true)
  }

  it should "work" in {
    val part1 = parseInput("day11.txt")
    for (i <- 1 to 20) {
      runRound(part1)
    }
    scoreMonkeys(part1) should be (117640)
  }

  "Really worry" should "process the example for 10000 rounds correctly" in {
    val example = parseInput(Example)
    for (i <- 1 to 10000) {
      runReallyWorryRound(example)
    }

    example.map(_.numInspection) should be(Seq(52166, 47830, 1938, 52013))

    scoreMonkeys(example) should be(2713310158L)
  }

  it should "work on the input data" in {
    val example = parseInput("day11.txt")
    for (i <- 1 to 10000) {
      runReallyWorryRound(example)
    }

    scoreMonkeys(example) should be(30616425600L)
  }

}
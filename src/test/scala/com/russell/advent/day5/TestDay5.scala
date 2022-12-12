package com.russell.advent.day5

import com.russell.advent.day5.Day5._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay5 extends AnyFlatSpec with should.Matchers {

  val Example = """    [D]
                  |[N] [C]
                  |[Z] [M] [P]
                  | 1   2   3
                  |
                  |move 1 from 2 to 1
                  |move 3 from 1 to 3
                  |move 2 from 2 to 1
                  |move 1 from 1 to 2""".stripMargin.split("\n")


  "Example data" should "parse properly" in {
    val (stacks, moves) = parseInput(Example)
    stacks.map(_.length).sum should be (6)
    moves.length should be (4)
  }

  it should "follow the part 1 example" in {
    val (stacks, moves) = parseInput(Example)
    applyMove(stacks, moves(0))

    "".concat(stacks(0)) should be ("DNZ")

    applyMove(stacks, moves(1))
    "".concat(stacks(2)) should be ("ZNDP")

    applyMove(stacks, moves(2))
    "".concat(stacks(0)) should be ("MC")

    applyMove(stacks, moves(3))
    "".concat(stacks.filter(_.length > 0).map(_.pop)) should be ("CMZ")
  }

  it should "follow the part 2 example" in {
    val (stacks, moves) = parseInput(Example)
    applyPart2Move(stacks, moves(0))

    "".concat(stacks(0)) should be("DNZ")

    applyPart2Move(stacks, moves(1))
    "".concat(stacks(2)) should be("DNZP")

    applyPart2Move(stacks, moves(2))
    "".concat(stacks(0)) should be("CM")

    applyPart2Move(stacks, moves(3))
    "".concat(stacks.filter(_.length > 0).map(_.pop)) should be("MCD")
  }

  "Part1" should "work" in {
    val (stacks, moves) = parseInput("day5.txt")
    for (move <- moves) {
      applyMove(stacks, move)
    }
    println("".concat(stacks.map(_.pop)))
  }

  "Part2" should "work" in {
    val (stacks, moves) = parseInput("day5.txt")
    for (move <- moves) {
      applyPart2Move(stacks, move)
    }
    println("".concat(stacks.map(_.pop)))
  }


}

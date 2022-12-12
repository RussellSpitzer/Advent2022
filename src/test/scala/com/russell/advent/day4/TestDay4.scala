package com.russell.advent.day4

import com.russell.advent.day4.Day4._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay4 extends AnyFlatSpec with should.Matchers {

  val Example = """2-4,6-8
                  |2-3,4-5
                  |5-7,7-9
                  |2-8,3-7
                  |6-6,4-6
                  |2-6,4-8""".stripMargin.split("\n")

  "Parse Input" should "parse valid ranges" in {
    val ranges = parseInput("day4.txt")
    ranges.length should not be 0
  }

  "Example" should "produce 6 pairs" in {
    val ranges = parseInput(Example)
    ranges.length should be (6)
  }

  it should "have 2 overlaps" in {
    val ranges = parseInput(Example)
    completeOverlap(ranges).length should be (2)
  }

  it should "have 4 partial overlaps" in {
    val ranges = parseInput(Example)
    partialOverlap(ranges).length should be(4)
  }


  "Part1" should "find pairs where one encloses the other" in {
    val ranges = parseInput("day4.txt")
    completeOverlap(ranges).length should not be 0
    print(completeOverlap(ranges).length)
  }

  "Part2" should "find pairs where one overlap the other" in {
    val ranges = parseInput("day4.txt")
    partialOverlap(ranges).length should not be 0
    print(partialOverlap(ranges).length)
  }


}

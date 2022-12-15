package com.russell.advent.day14

import com.russell.advent.AdventUtil.Position
import com.russell.advent.day14.Day14._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.Seq

class TestDay14 extends AnyFlatSpec with should.Matchers {



  val Example = """498,4 -> 498,6 -> 496,6
                  |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin.split("\n").map(_.trim)

  "Parse input" should "work with example" in {
    val walls = parseInput(Example)
    walls(0) should be (Seq(Position(498, 4), Position(498, 6), Position(496, 6)))
    walls(1) should be (Seq(Position(503, 4), Position(502, 4), Position(502, 9), Position(494, 9)))
  }

  "fill" should "fill the example" in {
    val walls = parseInput(Example)
    val sandPit = new SandPit(walls)
    sandPit.fill() should be (24)
    println(sandPit)
  }

  it should "fill the test data" in {
    val walls = parseInput("day14.txt")
    val sandPit = new SandPit(walls)
    sandPit.fill() should be(994)
    println(sandPit)
  }

  "infinite bottom fill" should "fill the example" in {
    val walls = parseInput(Example)
    val sandPit = new SandPit(walls)
    sandPit.fillBottomed() should be(93)
    println(sandPit)
  }

  it should "fill the test data" in {
    val walls = parseInput("day14.txt")
    val sandPit = new SandPit(walls)
    sandPit.fillBottomed() should be(26283)
    println(sandPit)
  }

}
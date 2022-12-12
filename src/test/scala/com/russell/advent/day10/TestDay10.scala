package com.russell.advent.day10

import com.russell.advent.day10.Day10.{WeirdCPU, parseInput}
import com.russell.advent.day9.Day9._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay10 extends AnyFlatSpec with should.Matchers {

  val Example = """addx 15
                  |addx -11
                  |addx 6
                  |addx -3
                  |addx 5
                  |addx -1
                  |addx -8
                  |addx 13
                  |addx 4
                  |noop
                  |addx -1
                  |addx 5
                  |addx -1
                  |addx 5
                  |addx -1
                  |addx 5
                  |addx -1
                  |addx 5
                  |addx -1
                  |addx -35
                  |addx 1
                  |addx 24
                  |addx -19
                  |addx 1
                  |addx 16
                  |addx -11
                  |noop
                  |noop
                  |addx 21
                  |addx -15
                  |noop
                  |noop
                  |addx -3
                  |addx 9
                  |addx 1
                  |addx -3
                  |addx 8
                  |addx 1
                  |addx 5
                  |noop
                  |noop
                  |noop
                  |noop
                  |noop
                  |addx -36
                  |noop
                  |addx 1
                  |addx 7
                  |noop
                  |noop
                  |noop
                  |addx 2
                  |addx 6
                  |noop
                  |noop
                  |noop
                  |noop
                  |noop
                  |addx 1
                  |noop
                  |noop
                  |addx 7
                  |addx 1
                  |noop
                  |addx -13
                  |addx 13
                  |addx 7
                  |noop
                  |addx 1
                  |addx -33
                  |noop
                  |noop
                  |noop
                  |addx 2
                  |noop
                  |noop
                  |noop
                  |addx 8
                  |noop
                  |addx -1
                  |addx 2
                  |addx 1
                  |noop
                  |addx 17
                  |addx -9
                  |addx 1
                  |addx 1
                  |addx -3
                  |addx 11
                  |noop
                  |noop
                  |addx 1
                  |noop
                  |addx 1
                  |noop
                  |noop
                  |addx -13
                  |addx -19
                  |addx 1
                  |addx 3
                  |addx 26
                  |addx -30
                  |addx 12
                  |addx -1
                  |addx 3
                  |addx 1
                  |noop
                  |noop
                  |noop
                  |addx -9
                  |addx 18
                  |addx 1
                  |addx 2
                  |noop
                  |noop
                  |addx 9
                  |noop
                  |noop
                  |noop
                  |addx -1
                  |addx 2
                  |addx -37
                  |addx 1
                  |addx 3
                  |noop
                  |addx 15
                  |addx -21
                  |addx 22
                  |addx -6
                  |addx 1
                  |noop
                  |addx 2
                  |addx 1
                  |noop
                  |addx -10
                  |noop
                  |noop
                  |addx 20
                  |addx 1
                  |addx 2
                  |addx 2
                  |addx -6
                  |addx -11
                  |noop
                  |noop
                  |noop""".stripMargin.split("\n").map(_.trim)

  val testIntervals = Seq(20,60,100,140,180,220)

  "Cpu" should "work with the example" in {
    val cpu = new WeirdCPU(parseInput(Example))
    testIntervals.map(ticks => cpu.outputAt(ticks)) should be (Seq(420, 1140, 1800, 2940, 2880, 3960))
  }

  it should "work with part1 data" in {
    val cpu = new WeirdCPU(parseInput("day10.txt"))
    val outputs = testIntervals.map(ticks => cpu.outputAt(ticks))
    println(outputs)
    println(outputs.sum)
  }

  it should "print the example design" in {
    val cpu = new WeirdCPU(parseInput(Example))
    cpu.genScreen().foreach(println)
  }

  it should "work with part2 data" in {
    val cpu = new WeirdCPU(parseInput("day10.txt"))
    cpu.genScreen().foreach(println)
  }


}

package com.russell.advent.day21

import com.russell.advent.day21.Day21._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay21 extends AnyFlatSpec with should.Matchers {

  val Example = """root: pppw + sjmn
                  |dbpl: 5
                  |cczh: sllz + lgvd
                  |zczc: 2
                  |ptdq: humn - dvpt
                  |dvpt: 3
                  |lfqf: 4
                  |humn: 5
                  |ljgn: 2
                  |sjmn: drzm * dbpl
                  |sllz: 4
                  |pppw: cczh / lfqf
                  |lgvd: ljgn * ptdq
                  |drzm: hmdt - zczc
                  |hmdt: 32""".stripMargin.split("\n").map(_.trim)

  "MonkeyGroup" should "work with example" in {
    val mg = parseInput(Example)
    mg.solveUntil("root") should be (152)
  }

  it should "work with input" in {
    val mg = parseInput("day21.txt")
    mg.solveUntil("root") should be (152479825094094L)
  }

  "Better translation" should "work with example" in {
    val mg = parseInput(Example)
    mg.solvePart2() should be (302)
  }

  it should "work with part2" in {
    val mg = parseInput("day21.txt")
    mg.solvePart2() should be
  }


}

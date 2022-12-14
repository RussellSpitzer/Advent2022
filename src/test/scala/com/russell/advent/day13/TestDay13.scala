package com.russell.advent.day13

import com.russell.advent.AdventUtil.Position
import com.russell.advent.AdventUtil.Position._
import com.russell.advent.day13.Day13._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay13 extends AnyFlatSpec with should.Matchers {



  val Example =
    """[1,1,3,1,1]
      |[1,1,5,1,1]
      |
      |[[1],[2,3,4]]
      |[[1],4]
      |
      |[9]
      |[[8,7,6]]
      |
      |[[4,4],4,4]
      |[[4,4],4,4,4]
      |
      |[7,7,7,7]
      |[7,7,7]
      |
      |[]
      |[3]
      |
      |[[[]]]
      |[[]]
      |
      |[1,[2,[3,[4,[5,6,7]]]],8,9]
      |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin.split("\n").map(_.trim)

  val Example2 = Example ++ Seq("[[2]]", "[[6]]")

  "ParseInput" should "work on the example" in {
    val example = parseInput(Example)
    example.size should be(23)
  }

  "Ordered" should "work on the example" in {
    val example = parseInput(Example)
    val pairs = example.grouped(2).toSeq
    val isOrdered = pairs.map(pair => Day13Order.compare(pair(0), pair(1)) < 0).toSeq
    isOrdered(0) should be(true)
    isOrdered(1) should be(true)
    isOrdered(2) should be(false)
    isOrdered(3) should be(true)
    isOrdered(4) should be(false)
    isOrdered(5) should be(true)
    isOrdered(6) should be(false)
    isOrdered(7) should be(false)
    isOrdered.zipWithIndex
      .filter { case (ordered, _) => ordered }
      .map { case (_, i) => i + 1 }
      .sum should be(13)
  }

  it should "work on part1 data" in {
    val example = parseInput("day13.txt")
    val pairs = example.grouped(2).toSeq
    val isOrdered = pairs.map(pair => Day13Order.compare(pair(0), pair(1)) < 0).toSeq
    isOrdered.zipWithIndex
      .filter { case (ordered, _) => ordered }
      .map { case (_, i) => i + 1 }
      .sum should be(5605)
  }

  val dividerTwo = parseString("[[2]]")
  val dividerSix = parseString("[[6]]")

  it should "sort the example" in {
    val example = parseInput(Example2).sorted(Day13Order)
    example(9) should be(dividerTwo)
    example(13) should be(dividerSix)
    val j = example.indexOf(dividerTwo) + 1
    val k = example.indexOf(dividerSix) + 1
    j * k should be(140)
  }

  it should "sort the input data" in {
    val example = (parseInput("day13.txt") ++ Seq(dividerTwo, dividerSix)).sorted(Day13Order)
    val j = example.indexOf(dividerTwo) + 1
    val k = example.indexOf(dividerSix) + 1
    j * k should be(24969)
  }
}
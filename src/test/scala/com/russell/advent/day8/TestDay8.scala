package com.russell.advent.day8

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Day8._

class TestDay8 extends AnyFlatSpec with should.Matchers {

  val Example =
    """30373
      |25512
      |65332
      |33549
      |35390
      |""".stripMargin.split("\n")

  "Example" should "parse correctly" in {
    val trees = parseTrees(Example)
    trees.length should be(5)
    trees(0).length should be(5)
  }

  "Tree visible" should "work with example" in {
    val trees = parseTrees(Example)
    treeVisible(2, 2, trees) should be (false)
    treeVisible(3, 1, trees) should be (false)
    treeVisible(3, 3, trees) should be (false)
    treesVisible(trees).length should be(21)
  }

  it should "work with part 1" in {
    val trees = parseTrees("day8.txt")
    println(treesVisible(trees).length)
  }

  "ViewFrom" should "work with example" in {
    val trees = parseTrees(Example)
    viewFrom(1,2, trees) should be (1, 2, 1, 2)
    viewFrom(3,2, trees) should be (2, 2, 2, 1)
  }

  "Scenic Score" should "work in example" in {
    scenicScore(parseTrees(Example)) should be (8)
  }

  it should "work with part2 data" in {
    val trees = parseTrees("day8.txt")
    println(scenicScore(trees))

  }


}

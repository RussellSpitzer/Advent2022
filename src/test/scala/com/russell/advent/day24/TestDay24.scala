package com.russell.advent.day24

import com.russell.advent.AdventUtil.Position
import com.russell.advent.day24.Day24._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay24 extends AnyFlatSpec with should.Matchers {
  val SimpleExample = """#.#####
                  |#.....#
                  |#>....#
                  |#.....#
                  |#...v.#
                  |#.....#
                  |#####.#""".stripMargin.split("\n")

  val Example = """#.######
                  |#>>.<^<#
                  |#.<..<<#
                  |#>v.><>#
                  |#<^v^^>#
                  |######.#""".stripMargin.split("\n")


  "Parse input" should "generate world for example" in {
    val world = parseInput(SimpleExample)
    world.blizzardsAtTime(0) should contain theSameElementsAs(Seq(Position(0,1), Position(3,3)))
  }

  "Blizzard propagation" should "work in small sample" in {
    val world = parseInput(SimpleExample)

    world.blizzardsAtTime(0) should contain theSameElementsAs(Seq(Position(0,1), Position(3,3)))
    world.blizzardsAtTime(1) should contain theSameElementsAs(Seq(Position(1,1), Position(3,4)))
    world.blizzardsAtTime(2) should contain theSameElementsAs(Seq(Position(2,1), Position(3,0)))
  }

  it should "work in a big example" in {
    val world = parseInput(Example)
    world.blizzardsAtTime(15) should contain(Position(0, 0))
    world.blizzardsAtTime(15) should contain(Position(1, 0))
    world.blizzardsAtTime(15) should contain(Position(2, 0))
    world.blizzardsAtTime(15) should contain(Position(3, 0))

    //world.display(18)
  }

  "Simulation" should "work in small example" in {
    val world = parseInput(SimpleExample)
    world.moveThroughBlizzards() should be (10)
    //world.display(11)

  }

  it should "work in example" in {
    val world = parseInput(Example)
    world.moveThroughBlizzards() should be(18)
    //world.display(18)
  }

  it should "work in input" in {
    val world = parseInput("day24.txt")
    world.moveThroughBlizzards() should be (247)
    //world.display(247)
  }

  it should "go back and forth in small example" in {
    val world = parseInput(SimpleExample)
    val firstLeg = world.moveThroughBlizzards()
    val secondLeg = world.moveThroughBlizzards(world.end, world.start.pos, firstLeg)
    val lastLeg = world.moveThroughBlizzards(time = secondLeg)
    firstLeg should be (10)
    secondLeg should be (20)
    lastLeg should be (30)
    //world.display(30)
  }

  it should "go back and forth in example" in {
    val world = parseInput(Example)
    val firstLeg = world.moveThroughBlizzards()
    val secondLeg = world.moveThroughBlizzards(world.end, world.start.pos, firstLeg)
    val lastLeg = world.moveThroughBlizzards(time = secondLeg)
    firstLeg should be(18)
    secondLeg should be(41)
    lastLeg should be(54)
    //world.display(54)
  }

  it should "go back and forth in input" in {
    val world = parseInput("day24.txt")
    val firstLeg = world.moveThroughBlizzards()
    val secondLeg = world.moveThroughBlizzards(world.end, world.start.pos, firstLeg)
    val lastLeg = world.moveThroughBlizzards(time = secondLeg)
    firstLeg should be(247)
    secondLeg should be(465)
    lastLeg should be(728)
    //world.display(728)
  }




}

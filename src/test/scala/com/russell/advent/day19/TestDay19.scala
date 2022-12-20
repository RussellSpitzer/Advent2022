package com.russell.advent.day19

import com.russell.advent.day19.Day19.Element._
import com.russell.advent.day19.Day19._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.collection.parallel.CollectionConverters._

class TestDay19 extends AnyFlatSpec with should.Matchers {

  val Example =
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
      .stripMargin.split("\n").map(_.trim)

  "Parse" should "work with the example" in {
    val plans = parseInput(Example)
    plans(0).costs should contain theSameElementsAs  (Array(
      ElementCollection(Array(4,0,0,0)),
      ElementCollection(Array(2,0,0,0)),
      ElementCollection(Array(3,14,0,0)),
      ElementCollection(Array(2,0,7,0))))
  }

  "World" should "simulate examples" in {
    val plans = parseInput(Example)
    val bestScores = plans.map {
      bestToBuild(_)
    }

    bestScores.zipWithIndex.map { case (score, planIdx) => score * (planIdx + 1)}.sum should be (33)
  }

  it should "simulate 32 minute examples" in {
    val plans = parseInput(Example)
    val bestScores = plans.map {
      bestToBuild(_, 32)
    }.toList

    bestScores should be (Seq(56, 62))
  }

  it should "simulate input" in {
    val plans = parseInput("day19.txt")
    val bestScores = plans.map {
      bestToBuild(_)
    }

    bestScores.zipWithIndex.map { case (score , planIdx) => score * (planIdx + 1) }.sum should be(1092)
  }

  it should "simulate input for 32 minutes" in {
    val plans = parseInput("day19.txt")
    val bestScores = plans.take(3).map { plan =>
      bestToBuild(plan, 32)
    }
    println(bestScores)
    bestScores.reduce(_ * _) should be (3542)
  }

  "Run test" should "simulate examples" in {
    val plans = parseInput(Example)

    World(plans(0))
      .run(Seq(Ore, Clay, Clay, Clay, Clay, Clay, Clay, Obsidian, Obsidian, Obsidian, Geode, Obsidian, Geode, Geode, Geode))
      .inventory(Geode) should be (8)
  }

  it should "simulate example2" in {
    val plans = parseInput(Example)

    World(plans(1))
      .run(Seq(Ore, Ore, Clay, Clay, Clay, Clay, Clay, Obsidian, Obsidian, Obsidian, Obsidian, Obsidian, Geode, Obsidian, Geode, Geode))
      .inventory(Geode) should be(12)
  }
}

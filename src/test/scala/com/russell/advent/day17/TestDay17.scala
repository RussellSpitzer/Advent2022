package com.russell.advent.day17

import com.russell.advent.day17.Day17._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay17 extends AnyFlatSpec with should.Matchers {

  val Example = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>""".stripMargin.split("\n").map(_.trim)

  "Parse Input" should "correctly parse example" in {
    val windMap = parseInput(Example)
  }

  "Game" should "play in the example" in {
    val windMap = parseInput(Example)
    val board = Game(windMap)
    while (board.numPiece != 2023) {
      if (board.numPiece < 6) {
        println(board)
      }
      board.dropShape()
    }
    board.lastHeight should be (3068)
  }

  it should "play in the input data" in {
    val windMap = parseInput("day17.txt")
    val board = Game(windMap)
    while (board.numPiece != 2023) {
      if (board.numPiece < 6) {
        println(board)
      }
      board.dropShape()
    }
    board.lastHeight should be(3166)
  }

  it should "play in the very long example" in {
    val windMap = parseInput(Example)
    val board = Game(windMap)

    // Simulate enough to find the period
    while (board.numPiece != 10000) {
      board.dropShape()
    }
    val (blockPeriod, periodHeight) = board.findCycle()

    val simulatedPieces = board.numPiece

    println(s"Detected a period of ${blockPeriod}, which adds a height of ${periodHeight}")

    val piecesLeftToSimulate = 1000000000001L - simulatedPieces
    val periodsLeftToSimulate = piecesLeftToSimulate / blockPeriod

    val piecesLeft = piecesLeftToSimulate % blockPeriod
    val simulationTargetEnd = board.numPiece + piecesLeft
    println(s"Off by ${piecesLeft}, simulating now to ${simulationTargetEnd}")
    while (board.numPiece != simulationTargetEnd) {
      board.dropShape()
    }

    val theoreticalHeight = board.lastHeight + periodsLeftToSimulate * periodHeight

    theoreticalHeight should be(1514285714288L)
  }

  it should "play in the very long input" in {
    val windMap = parseInput("day17.txt")
    val board = Game(windMap)

    // Simulate enough to find the period
    while (board.numPiece != 100000) {
      board.dropShape()
    }
    val (blockPeriod, periodHeight) = board.findCycle()

    val simulatedPieces = board.numPiece

    println(s"Detected a period of ${blockPeriod}, which adds a height of ${periodHeight}")

    val piecesLeftToSimulate = 1000000000001L - simulatedPieces
    val periodsLeftToSimulate = piecesLeftToSimulate / blockPeriod

    val piecesLeft = piecesLeftToSimulate % blockPeriod
    val simulationTargetEnd = board.numPiece + piecesLeft
    println(s"Off by ${piecesLeft}, simulating now to ${simulationTargetEnd}")
    while (board.numPiece != simulationTargetEnd) {
      board.dropShape()
    }

    val theoreticalHeight = board.lastHeight.toLong + periodsLeftToSimulate * periodHeight.toLong

    // 1577207977184 to low
    theoreticalHeight should be(1577207977186L)
  }



}

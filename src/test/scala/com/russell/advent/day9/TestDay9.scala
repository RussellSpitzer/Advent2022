package com.russell.advent.day9

import com.russell.advent.day9.Day9._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay9 extends AnyFlatSpec with should.Matchers {

  val Example =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin.split("\n").map(_.trim)

  val Example2 = """R 5
                   |U 8
                   |L 8
                   |D 3
                   |R 17
                   |D 10
                   |L 25
                   |U 20""".stripMargin.split("\n").map(_.trim)

  "Example" should "parse correctly" in {
    val moves = parseMoves(Example)
    moves.length should be(8)
  }

  it should "have correct tail moves" in {
    val moves = parseMoves(Example)
    moveTail(Origin, moveHead(Origin, moves)).distinct.length should be (13)
  }

  "Moves" should "work" in {
    val headPositions = moveHead(Origin, Move(Direction.U, 3))
    headPositions should be(Seq(Position(1,2), Position(1,3), Position(1,4)))
    val tailPositions = moveTail(Origin, headPositions)
    tailPositions should be(Seq(
      Position(1,1),
      Position(1,2),
      Position(1,3)
    ))
  }

  "Part1" should "move" in {
    val moves = parseMoves("day9.txt")
    val headPositions = moveHead(Origin, moves)
    val tailPositions = moveTail(Origin, headPositions)
    tailPositions.distinct.length should be (6057)
  }

  "Example2" should "work" in {
    val moves = parseMoves(Example2)
    val headPositions = moveHead(Origin, moves)
    val finalTailPositions = (1 to 9).foldLeft(headPositions)((headPositions, _) =>
      moveTail(Origin, headPositions))
    finalTailPositions.distinct.length should be (36)
  }



  "Part2" should "move nine tails" in {
    val moves = parseMoves("day9.txt")
    val headPositions = moveHead(Origin, moves)
    val finalTailPositions = (1 to 9).foldLeft(headPositions)((headPositions,_) =>
      moveTail(Origin, headPositions))
    finalTailPositions.distinct.length should be (2514)
  }


}

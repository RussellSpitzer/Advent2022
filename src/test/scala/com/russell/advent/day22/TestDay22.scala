package com.russell.advent.day22

import com.russell.advent.AdventUtil.Position
import com.russell.advent.day22.Day22.Direction._
import com.russell.advent.day22.Day22._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Left

class TestDay22 extends AnyFlatSpec with should.Matchers {

  val Example ="""        ...#
                 |        .#..
                 |        #...
                 |        ....
                 |...#.......#
                 |........#...
                 |..#....#....
                 |..........#.
                 |        ...#....
                 |        .....#..
                 |        .#......
                 |        ......#.
                 |
                 |10R5L5R10L4R5L5""".stripMargin.split("\n").toSeq


  "Parse Input" should "work with example" in {
    val (board, move) = parseInput(Example)
    move.last.swap.toOption.get should be (5)
    board.walls should contain (Position(3,4))
  }

  "Run moves" should "work with example" in {
    val (board, moves) = parseInput(Example)
    board.runMoves(moves)
    println(board)
    board.score() should be (6032)
  }

  it should "work with input" in {
    val (board, moves) = parseInput("day22.txt")
    board.runMoves(moves)
    println(board)
    board.score() should be(191010)
  }

  "Cube" should "test bounds" in {
    val (board, moves) = parseInput("day22_dummy.txt")
    // A UP
    board.currentPosition = Position(125, 2)
    board.currentDirection = UP
    board.runMovesCube(Seq(Left(10)))
    board.currentPosition should be (Position(25, 192))

    // A RIGHT
    board.currentPosition = Position(147, 20)
    board.currentDirection = RIGHT
    board.runMovesCube(Seq(Left(10)))
    board.currentPosition should be(Position(92, 129))

    // B UP
    board.currentPosition = Position(75, 2)
    board.currentDirection = UP
    board.runMovesCube(Seq(Left(10)))
    board.currentPosition should be (Position(7, 175) )

    // C RIGHT
    board.currentPosition = Position(97, 75)
    board.currentDirection = RIGHT
    board.runMovesCube(Seq(Left(10)))
    board.currentPosition should be(Position(125, 42))

    // C LEFT
    board.currentPosition = Position(51, 75)
    board.currentDirection = LEFT
    board.runMovesCube(Seq(Left(10)))
    board.currentPosition should be(Position(25, 108))
    println(board)
  }


  it should "do loops" in {
    val (board, moves) = parseInput("day22_dummy.txt")
    board.currentPosition = Position(125, 25)
    board.currentDirection = UP
    board.runMovesCube(Seq(Left(200)))
    board.currentPosition should be (Position(125,25))
    board.currentDirection should be (UP)

    board.currentPosition = Position(130, 25)
    board.currentDirection = DOWN
    board.runMovesCube(Seq(Left(200)))
    board.currentPosition should be(Position(130, 25))
    board.currentDirection should be(DOWN)

    board.currentPosition = Position(125, 30)
    board.currentDirection = RIGHT
    board.runMovesCube(Seq(Left(200)))
    board.currentPosition should be(Position(125, 30))
    board.currentDirection should be(RIGHT)

    board.currentPosition = Position(125, 20)
    board.currentDirection = LEFT
    board.runMovesCube(Seq(Left(200)))
    board.currentPosition should be(Position(125, 20))
    board.currentDirection should be(LEFT)

    println(board)
  }

  it should "work with the input" in {
    val (board, moves) = parseInput("day22.txt")
    board.runMovesCube(moves)
    println(board)
    board.score() should be (55364)
  }

}

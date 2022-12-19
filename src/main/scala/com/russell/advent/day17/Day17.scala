package com.russell.advent.day17

import com.russell.advent.AdventUtil.{Position}

import scala.collection.mutable
import scala.io.Source

object Day17 {

  def parseInput(path: String): Seq[Char] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines: Seq[String]): Seq[Char] = {
    lines(0)
  }

  case class TetrisPiece(position: Position, shape: Seq[Position]) {
    def currentLocation: Seq[Position] = {
      shape.map( x => x + position)
    }
  }

  case class Game(gas: Seq[Char]) {
    val HorizontalLine = Seq(Position(0, 0), Position(1, 0), Position(2, 0), Position(3, 0))
    val Cross = Seq(Position(1, 0), Position(0, 1), Position(1, 1), Position(2, 1), Position(1, 2))
    val LPiece = Seq(Position(0, 0), Position(1, 0), Position(2, 0), Position(2, 1), Position(2, 2))
    val VerticalLine = Seq(Position(0, 0), Position(0, 1), Position(0, 2), Position(0, 3))
    val Square= Seq(Position(0, 0), Position(1, 0), Position(1, 1), Position(0, 1))

    val shapeSequence = IndexedSeq(HorizontalLine, Cross, LPiece, VerticalLine, Square)

    var numPiece = 0
    var gasIndex = 0
    var lastHeight = 0
    var newPiece = false
    val cycleStarts: Array[Int] = Array.fill(gas.size)(0)

    var currentPiece: TetrisPiece = nextShape()
    var filledSpaces: mutable.Map[Position, Char] = mutable.Map().withDefaultValue('.')

    def nextShape(): TetrisPiece = {
      newPiece = true
      val piece = TetrisPiece(Position(3, lastHeight + 4), shapeSequence(numPiece % 5))
      numPiece += 1
      if (numPiece % 5 == 0) {
        cycleStarts(gasIndex) = cycleStarts(gasIndex) + 1
      }
      piece
    }

    def findCycle(): (Int, Int) = {
      val cycleStart = cycleStarts.zipWithIndex.maxBy(_._1)._2
      val shapeIdx = 0

      val blockDiffOne = runUntilPieceWithGas(shapeIdx, cycleStart)
      dropShape()
      val blockDiffTwo = runUntilPieceWithGas(shapeIdx, cycleStart)
      dropShape()
      val heightBefore = lastHeight
      dropShape()
      val blockDiffThree = runUntilPieceWithGas(shapeIdx, cycleStart)
      val heightDiff = lastHeight - heightBefore
      println(s"Period estimate for Shape $shapeIdx at ${cycleStart} : ($blockDiffOne, $blockDiffTwo, $blockDiffThree) : Height: $heightDiff")
      (blockDiffThree, heightDiff)
    }

    def runUntilPieceWithGas(shapeIdx: Int, gasStart: Int): Int = {
      val startPiece = numPiece
      while (gasIndex != gasStart || numPiece % 5 != shapeIdx || !newPiece) {
        if (newPiece && gasIndex == gasStart) {
          println(s"Run until $numPiece, no cycle found, piece is ${(numPiece % 5)}")
        }
        dropShape()
      }
      numPiece - startPiece
    }


    def dropShape(): Long = {
      val direction = gas(gasIndex)
      gasIndex = if (gasIndex == gas.size - 1) 0 else gasIndex + 1
      val gasShifted = direction match {
        case '<' =>
          currentPiece.copy(position = currentPiece.position.copy(x = currentPiece.position.x - 1))
        case '>' =>
          currentPiece.copy(position = currentPiece.position.copy(x = currentPiece.position.x + 1))
      }
      val gasResult = if (gasShifted.currentLocation.forall(pos => pos.x >= 1 && pos.x <= 7 && !filledSpaces.contains(pos))) {
        gasShifted
      } else {
        currentPiece
      }
      val gravityShifted = gasResult.copy(position =  gasResult.position.copy(y =gasResult.position.y -1))
      if (gravityShifted.currentLocation.forall(pos => pos.y >= 1 && !filledSpaces.contains(pos))) {
        currentPiece = gravityShifted
        newPiece = false
        numPiece
      } else {
        // Piece settled
        val settledLocation = gasResult.currentLocation
        settledLocation.foreach(filledSpaces(_) = '#')

        val lowestY = settledLocation.minBy(_.y).y

        (lowestY to lowestY + 2).find( y =>
          (1 to 7).forall(x => filledSpaces.contains(Position(x, y)))
        ).foreach( tetrisY =>
          filledSpaces.keys.filter(pos => pos.y < tetrisY - 1).map(filledSpaces.remove)
        )

        lastHeight = Math.max(lastHeight, settledLocation.maxBy(_.y).y)
        currentPiece = nextShape()
        numPiece
      }
    }

    override def toString: String = {
      (lastHeight + 6 to 1 by -1).map { y =>
        "|" + (1 to 7).map(x => {
          if (currentPiece.currentLocation.contains(Position(x, y))) {
            "@"
          } else {
            filledSpaces(Position(x, y))
          }
        }).mkString("") + "|"
      }.mkString("\n") + "\n---------\n"
    }
  }

}

package com.russell.advent.day22

import com.russell.advent.AdventUtil.Position

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day22 {

  def parseInput(path: String): (Board, Seq[Either[Int, Char]]) = {
    parseInput(Source.fromResource(path).getLines().toSeq)
  }

  def parseInput(lines: Seq[String]): (Board, Seq[Either[Int, Char]]) = {
    val MapChar = Set(' ', '.', '#')
    val WallOrGround = MapChar - ' '
    val wallBuilder = Set.newBuilder[Position]
    val xRanges = IndexedSeq.newBuilder[Range.Inclusive]
    var y = 0
    lines
      .takeWhile( line => line.nonEmpty && MapChar.contains(line(0)))
      .foreach{ case line =>
        xRanges.addOne(line.indexWhere(WallOrGround.contains(_)) to line.lastIndexWhere(WallOrGround.contains(_)))
        wallBuilder.addAll(line.zipWithIndex
          .filter( x => x._1 == '#')
          .map(_._2)
          .map(x => Position(x, y)))
        y += 1
      }

    val lastLine = lines.last
    val moves = lastLine
      .replaceAll("R", "-R-")
      .replaceAll("L", "-L-")
      .split("-")
      .map(entry =>
        Try(entry.toInt).toEither.swap.map(_ => entry(0))
      )
    (Board(xRanges.result(), wallBuilder.result()), moves)
  }

  object Direction extends Enumeration {
    type Direction = Value
    val UP, DOWN, LEFT, RIGHT = Value

    def character(direction: Direction) = {
      direction match {
        case UP => '^'
        case DOWN => 'v'
        case RIGHT => '>'
        case LEFT => '<'
      }
    }

    def turnRight(direction: Direction): Direction = direction match {
      case UP => RIGHT
      case RIGHT => DOWN
      case DOWN => LEFT
      case LEFT => UP
    }

    def turnLeft(direction: Direction): Direction = direction match {
      case UP => LEFT
      case RIGHT => UP
      case DOWN => RIGHT
      case LEFT => DOWN
    }
  }
  import Direction._

  case class Board(xRanges: IndexedSeq[Range.Inclusive], walls: Set[Position]) {
    val pathHistory: mutable.Map[Position, Char] = mutable.Map.empty[Position, Char]
    val (minX, maxX) = (xRanges.map(_.min).min, xRanges.map(_.max).max)
    val yRanges: IndexedSeq[Range.Inclusive] = (minX to maxX).map(columnId => {
      val present = xRanges.zipWithIndex.filter(range => range._1.contains(columnId))
      present.map(_._2).min to present.map(_._2).max
    })

    var currentPosition = Position(xRanges(0).min, 0)
    var currentDirection = RIGHT

    def runMoves(moves: Seq[Either[Int, Char]]): Unit = {
      moves.foreach { m =>
        if (m.isLeft) {
          currentPosition = move(currentPosition, currentDirection, m.swap.toOption.get)
        } else {
          m.toOption.get match {
            case 'R' => currentDirection = turnRight(currentDirection)
            case 'L' => currentDirection = turnLeft(currentDirection)
          }
        }
      }
    }

    def runMovesCube(moves: Seq[Either[Int, Char]]): Unit = {
      moves.foreach { m =>
        if (m.isLeft) {
          currentPosition = moveCube(currentPosition, currentDirection, m.swap.toOption.get)
        } else {
          m.toOption.get match {
            case 'R' => currentDirection = turnRight(currentDirection)
            case 'L' => currentDirection = turnLeft(currentDirection)
          }
        }
      }
    }

    def rangesAtPosition(position: Position): (Range.Inclusive, Range.Inclusive) = {
      (xRanges(position.y), yRanges(position.x))
    }

    def move(position: Position, direction: Direction, numMoves: Int): Position = {
      val (xRange: Range.Inclusive, yRange: Range.Inclusive) = rangesAtPosition(position)
      val (xSize, ySize) = (xRange.size, yRange.size)
      if (numMoves == 0) {
        position
      } else {
        val newPosition = direction match {
          case UP => position.copy(y = ((position.y - yRange.start) - 1 + ySize) % ySize + yRange.start)
          case DOWN => position.copy(y = ((position.y - yRange.start) + 1 + ySize) % ySize + yRange.start)
          case RIGHT => position.copy(x = ((position.x - xRange.start) + 1 + xSize) % xSize + xRange.start)
          case LEFT => position.copy(x = ((position.x - xRange.start) - 1 + xSize) % xSize + xRange.start)
        }
        if (walls.contains(newPosition)) {
          position
        } else {
          pathHistory.addOne(newPosition, character(direction))
          move(newPosition, direction, numMoves - 1)
        }
      }
    }

    def moveCube(position: Position, direction: Direction, numMoves: Int): Position = {
      if (numMoves == 0) {
        currentDirection = direction
        position
      } else {
        val newPosition = direction match {
          case UP => position.copy(y = position.y - 1)
          case DOWN => position.copy(y = position.y + 1)
          case RIGHT => position.copy(x = position.x + 1)
          case LEFT => position.copy(x =  position.x -1)
        }
        var newDirection = direction
        val rollOverPosition: Position = {
          if (newPosition.y == -1 && (50 to 149).contains(newPosition.x)) {
            if ((50 to 99).contains(newPosition.x)) {
              // B Up
              newDirection = RIGHT
              newPosition.copy(0, newPosition.x + 100)
            } else {
              // A Up
              newDirection = UP
              newPosition.copy(x= newPosition.x - 100, y = 199)
            }
          } else if (newPosition.y == 50 && (100 to 149).contains(newPosition.x)) {
            // A Down
            newDirection = LEFT
            newPosition.copy(99, newPosition.x - 50)

          } else if (newPosition.x == 150 && ((0 to 49).contains(newPosition.y))) {
              // A Right
              newDirection = LEFT
              newPosition.copy(x = 99, 149 - newPosition.y)
          } else if (newPosition.x == 100 && (50 to 149).contains(newPosition.y)) {
            if ((50 to 99).contains(newPosition.y)) {
              // C Right
              newDirection = UP
              newPosition.copy(x = newPosition.y + 50, 49)
            } else {
              // D Right
              newDirection = LEFT
              newPosition.copy(149, 149 - newPosition.y)
            }
          } else if (newPosition.y == 150 && (50 to 99).contains(newPosition.x)) {
            // D Down
            newDirection = LEFT
            newPosition.copy(49, newPosition.x + 100)
          } else if (newPosition.x == 50 && (150 to 199).contains(newPosition.y)) {
            // F Right
            newDirection = UP
            newPosition.copy(newPosition.y - 100, 149)
          } else if (newPosition.y == 200) {
            //F Down
            newDirection = DOWN
            newPosition.copy(newPosition.x + 100, 0)
          } else if (newPosition.x == -1) {
            if ((100 to 149).contains(newPosition.y)) {
              // E Left
              newDirection = RIGHT
              newPosition.copy(50,  149 - newPosition.y)
            } else {
              //F Left
              newDirection = DOWN
              newPosition.copy(newPosition.y - 100, 0)
            }
          } else if (newPosition.y == 99 && (0 to 49).contains(newPosition.x)) {
            // E Up
            newDirection = RIGHT
            newPosition.copy(50, newPosition.x + 50)
          } else if (newPosition.x == 49 && (0 to 99).contains(newPosition.y)) {
            if ((0 to 49).contains(newPosition.y)) {
              // B Left
              newDirection = RIGHT
              newPosition.copy(0, 149 - newPosition.y)
            } else {
              // C Left
              newDirection = DOWN
              newPosition.copy(newPosition.y - 50, 100)
            }
          } else {
            // No Rollover
            newPosition
          }
        }

        if (walls.contains(rollOverPosition)) {
          currentDirection = direction
          position
        } else {
          pathHistory.addOne(rollOverPosition, character(newDirection))
          moveCube(rollOverPosition, newDirection, numMoves - 1)
        }
      }
    }

    def score(): Int = {
      val row = currentPosition.y + 1
      val col = currentPosition.x + 1
      val facing = currentDirection match {
        case RIGHT => 0
        case DOWN => 1
        case LEFT => 2
        case UP => 3
      }
      1000 * row + 4 * col + facing
    }

    override def toString: String = {
      for (y <- 0 until xRanges.size) yield {
        (0 until maxX) .map { x =>
          val pos = Position(x,y)
          if (walls.contains(pos)) {
            '#'
          } else if (pos == currentPosition) {
            character(currentDirection)
          } else if (pathHistory.contains(pos)) {
            pathHistory(pos)
          } else if (xRanges(y).contains(x)) {
            '.'
          } else {
            ' '
          }
        }
      }.mkString("")
    }.mkString("\n")
  }
}

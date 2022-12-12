package com.russell.advent.day9

import scala.io.Source

object Day9 {

  object Direction extends Enumeration {
    type Direction = Value
    val U, D, L, R = Value
  }
  import Direction._

  case class Move(direction: Direction, length: Int)
  case class Position(x: Int, y: Int)

  val Origin = Position(1,1)

  def parseMoves(path: String): Seq[Move] = {
    parseMoves(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseMoves(lines: Seq[String]): Seq[Move] = {
    lines.map { line =>
      val split = line.split(" ")
      Move(Direction.withName(split(0)), split(1).toInt)
    }
  }

  def moveHead(position: Position, direction: Direction): Position = {
   direction match {
      case U => position.copy(y = position.y + 1)
      case D => position.copy(y = position.y - 1)
      case L => position.copy(x = position.x - 1)
      case R => position.copy(x = position.x + 1)
    }
  }

  def adjustTail(head: Position, tail: Position): Position = {
    val (diffx, diffy) = (head.x - tail.x, head.y - tail.y)
    if (Math.abs(diffx) == 2 || Math.abs(diffy) == 2) {
      tail.copy(tail.x + diffx.sign, tail.y + diffy.sign)
    } else {
      tail
    }
  }


  def moveHead(position: Position, move: Move): Seq[Position] = {
    var currentPosition = position
    (0 until move.length).map{ i =>
      currentPosition = moveHead(currentPosition, move.direction)
      currentPosition.copy()
    }
  }

  def moveHead(position: Position, moves: Seq[Move]): Seq[Position] = {
    var currentPosition = position
    Seq(Origin) ++ moves.flatMap { x =>
      val positions = moveHead(currentPosition, x)
      currentPosition = positions(positions.length - 1)
      positions
    }
  }

  def moveTail(originalTail: Position, headPositions: Seq[Position]): Seq[Position] = {
    var tailPosition = originalTail
    headPositions.map { head =>
      tailPosition = adjustTail(head, tailPosition)
      tailPosition
    }
  }




}

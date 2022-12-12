package com.russell.advent.day12

import scala.io.Source
import com.russell.advent.AdventUtil._
object Day12 {

  case class World(start: Position, end: Position, board: XYArray[Int]) {
    def canTraverse(from: Position, to: Position): Boolean = {
      if (!board.inBounds(from) || !board.inBounds(to)) {
        false
      } else {
        val fromHeight = board(from)
        val toHeight = board(to)
        fromHeight >= toHeight - 1
      }
    }
    def apply(pos: Position): Int = {
      board(pos)
    }
  }

  def parseInput(path: String): World = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }
  def parseInput(lines: Seq[String]): World = {
    var start: Position = Position(0, 0)
    var end: Position = Position(0, 0)
    val board = lines.zipWithIndex.map {
      case (str: String, y: Int) => str.zipWithIndex.map {
        case (char: Char, x: Int) => char match {
          case 'S' =>
            start = Position(x, y);
            0
          case 'E' =>
            end = Position(x, y)
            'z'.toInt - 'a'.toInt
          case other =>
            other.toInt - 'a'.toInt
        }
      }
    }.toIndexedSeq
    World(start, end, XYArray(board))
  }



  val Unset = 0
  def populatePaths(world: World, paths: XYArray[Int], dest: Position): Unit = {
    val pathValue = paths(dest)
    dest.adjacent()
      .filter(origin => origin != world.end && world.canTraverse(origin, dest))
      .foreach(origin => {
        val curValue = paths(origin)
        if (curValue == 0 || curValue > pathValue + 1) {
          paths(origin) = pathValue + 1
          populatePaths(world, paths, origin)
        }
      })
  }
  def findMinPathToStart(world: World): XYArray[Int] = {
    val paths: XYArray[Int] = XYArray(world.board.dimx, world.board.dimy)
    populatePaths(world, paths, world.end)
    paths
  }

}

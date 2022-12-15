package com.russell.advent.day14

import com.russell.advent.AdventUtil.Position
import com.russell.advent.AdventUtil.Position.between

import scala.collection.mutable
import scala.io.Source

object Day14 {

  def parseInput(path: String): Seq[Seq[Position]] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines: Seq[String]): Seq[Seq[Position]] = {
    lines.map(line => line.split(" -> "))
      .map { pairs =>
        pairs.map { pair =>
          val posStr = pair.split(",")
          Position(posStr(0).toInt, posStr(1).toInt)
        }
      }
  }

  class SandPit(val walls: Seq[Seq[Position]]) {
    val sandLocations: mutable.Map[Position, Char] = mutable.Map().withDefaultValue('.')
    val bottomDepth = walls.flatten.maxBy(_.y).y
    val SandStart = Position(500, 0)

    for (wall <- walls) {
      wall.sliding(2).foreach {
        case segment =>
          val (start, end) = (segment(0), segment(1))
          between(start, end).foreach(pos =>
            sandLocations(pos) = '#'
          )
      }
    }

    override def toString: String = {
      val minDrawX = walls.flatten.minBy(_.x).x - 1
      val maxDrawX = walls.flatten.maxBy(_.x).x + 1
      val rows = for (y <- (SandStart.y to bottomDepth + 1)) yield {
        between(Position(minDrawX, y), Position(maxDrawX, y)).map(sandLocations).mkString("")
      }
      rows.mkString("\n")
    }

    def fill(): Int = {
      var count = 0
      println(this)
      println("\n\n")
      while (addSand()) {
        count +=1
      }
      count
    }

    def fillBottomed(): Int = {
      var count  = 0
      println(this)
      println("\n\n")
      while (addSand(infiniteBottom = true) && !sandLocations.contains(SandStart)) {
        count += 1
      }
      count + 1
    }


    def addSand(infiniteBottom: Boolean = false): Boolean = {
      var sandPosition = SandStart
      while (sandPosition.y < bottomDepth + 1) {
        if (sandLocations.contains(sandPosition.up())) {
          if (sandLocations.contains(sandPosition.up().left())) {
            if(sandLocations.contains(sandPosition.up().right())) {
              sandLocations(sandPosition) = 'o'
              return true
            } else {
              sandPosition = sandPosition.up().right()
            }
          } else {
            sandPosition = sandPosition.up().left()
          }
        } else {
          sandPosition = sandPosition.up()
        }
      }
      if (infiniteBottom) {
        sandLocations(sandPosition) = 'o'
        true
      } else {
        false
      }
    }
  }
}

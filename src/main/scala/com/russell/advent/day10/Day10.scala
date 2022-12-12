package com.russell.advent.day10

import scala.collection.mutable
import scala.io.Source

object Day10 {

  def parseInput(path: String): Seq[Int] = {
    parseInput(Source.fromResource(path).getLines.map(_.trim).toSeq)
  }

  def parseInput(input: Seq[String]): Seq[Int] = {
    input.flatMap {
      case "noop" => Seq(0)
      case str => Seq(0, str.split(" ")(1).toInt)
    }
  }

  class WeirdCPU(instructions: Seq[Int]) {
    def outputAt(tick: Int): Int = {
      xAt(tick) * tick
    }

    def xAt(tick: Int): Int = {
      instructions.take(tick - 1).sum + 1
    }

    def genScreen(): Seq[String]  = {
      val output = for (row <- 0 to 5) yield {
        (1 to 40).map{ col =>
          val tick = row * 40 + col
          val spritePosition = xAt(tick) + 1
          if (Math.abs(col - spritePosition) <= 1) {
            '#'
          } else {
            ' '
          }
        }.mkString("")
      }
      output
    }
  }

}

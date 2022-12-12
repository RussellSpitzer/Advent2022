package com.russell.advent.day4

import scala.io.Source

object Day4 {

  def parseInput(path: String): Seq[(Range.Inclusive, Range.Inclusive)] = {
    parseInput(Source.fromResource(path).getLines().toSeq)
  }

  def parseInput(lines: Seq[String]) : Seq[(Range.Inclusive, Range.Inclusive)] = {
    lines.map(_.trim)
      .map(_.split(","))
      .map { pair =>
        val ranges = pair.map { range =>
          val split = range.split("-")
          (split(0).toInt to split(1).toInt)
        }
        (ranges(0), ranges(1))
      }
  }

  def completeOverlap(ranges : Seq[(Range, Range)]) : Seq[(Range, Range)] = {
    ranges.filter { pair =>
      val intersection = pair._1.intersect(pair._2).length
      intersection == pair._1.length || intersection == pair._2.length
    }
  }

  def partialOverlap(ranges: Seq[(Range, Range)]): Seq[(Range, Range)] = {
    ranges.filter { pair =>
     pair._1.intersect(pair._2).length > 0
    }
  }

}

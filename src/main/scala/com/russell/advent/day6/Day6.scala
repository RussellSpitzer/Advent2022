package com.russell.advent.day6

import scala.io.Source

object Day6 {

  def findFirstDifferentN(s: String, numDiff: Int = 4): Option[(String, Int)] = {
    s.sliding(numDiff)
      .zipWithIndex
      .find { case (str, _) => str.distinct.length == numDiff}
      .map {case (str, idx) => (str, idx + numDiff)}
  }

  def parseInput(path: String): String = {
    Source.fromResource(path).getLines().map(_.trim).next()
  }

}

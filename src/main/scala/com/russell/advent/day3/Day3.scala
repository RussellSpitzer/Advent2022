package com.russell.advent.day3

import scala.io.Source

object Day3 {

  def parseInput(path: String): Seq[(String, String)] = {
    Source.fromResource(path).getLines().map(_.trim).map { line =>
      (line.substring(0, line.length/2), line.substring(line.length/2))
    }.toSeq
  }

  def parseInputPart2(path: String) : Seq[String] = {
    Source.fromResource(path).getLines().map(_.trim).toSeq
  }

  def commonElement(first: String, second: String) = {
    first.intersect(second)
  }

  def scoreElement(element: Char): Int = {
    val LowerCaseA: Int = 'a'.toInt - 1
    val UpperCaseA: Int = 'A'.toInt - 1
    val value = element.toInt
    if (value > LowerCaseA) {
      value - LowerCaseA
    } else {
      value - UpperCaseA + 26
    }
  }

  def findTriples(commonChar: Char,  teams: Seq[String]): Boolean = {
    val common = teams.filter(_.contains(commonChar))
    common.length >= 3
  }

  def allTriples(teams: Seq[String]) = {
    teams.grouped(3).map { triple =>
      ('A' to 'z')
        .filter(char => findTriples(char, triple))
        .map(scoreElement(_))
        .sum
    }.sum
  }



}

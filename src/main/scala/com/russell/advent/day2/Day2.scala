package com.russell.advent.day2

import scala.io.Source



object Day2 {

  val Part1Map = Map(
      "X" -> "A",
      "Y" -> "B",
      "Z" -> "C"
    )

  val Part1Strategy = (_: String, player: String) => Part1Map(player)

  def Part2Strategy (opponent: String, outcome: String): String = {
    outcome match  {
      case "X" => Lose(opponent)
      case "Y" => opponent
      case "Z" => Win(opponent)
    }
  }

  val Win = Map (
    "A" -> "B",
    "B" -> "C",
    "C" -> "A"
  )

  val Lose = Win.map(_.swap)

  val Extra = Map (
    "A" -> 1,
    "B" -> 2,
    "C" -> 3
  )

  def score(opponent: String, player: String) : Int = {
    if (opponent == player) {
      Extra(player) + 3
    } else if (Win(opponent) == player) {
      Extra(player) + 6
    } else {
      Extra(player)
    }
  }

  def parseInput(path: String) : Seq[(String, String)] = {
    Source.fromResource(path).getLines().map { line =>
      val splitString = line.trim.split(" ")
      (splitString(0), splitString(1))
    }.toSeq
  }

  def scoreMatches(matches: Seq[(String, String)], strategyGuide: (String, String) => String): Int = {
    matches.map(m => score(m._1, strategyGuide(m._1, m._2))).sum
  }

}

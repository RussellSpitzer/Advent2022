package com.russell.advent.day1

import scala.collection.mutable
import scala.io.Source

object Day1 {

  def parseInput(path: String): Map[Int, Seq[Int]] = {
    var index = 1;
    val elves = new mutable.HashMap[Int, Seq[Int]]().withDefault( _ => Seq())
    for (line <- Source.fromResource(path).getLines()) line match {
      case "" => index += 1
      case number: String => elves(index) = elves(index) :+ number.toInt
    }
    elves.toMap
  }
}

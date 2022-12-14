package com.russell.advent.day13

import com.russell.advent.AdventUtil._
import io.circe.Json

import scala.io.Source

object Day13 {

  def parseInput(path: String): Seq[Json] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines: Seq[String]): Seq[Json] = {
    lines.filter(!_.isBlank).map(parseString)
  }

  def parseString(line: String): Json = {
    io.circe.parser.parse(line) match {
      case Left(value) => Json.Null
      case Right(value) => value
    }
  }

  object Day13Order extends Ordering[Json] {

    override def compare(jsonLeft: Json, jsonRight: Json): Int = {
      if (jsonLeft.isNumber && jsonRight.isNumber) {
        jsonLeft.asNumber.get.toInt.get - jsonRight.asNumber.get.toInt.get
      } else if (jsonLeft.isArray && jsonRight.isArray) {
        val leftArray = jsonLeft.asArray.get
        val rightArray = jsonRight.asArray.get
        for (idx <- 0 until rightArray.length) {
          if (idx >= leftArray.size) {
            return -1
          } else {
            val compared =  compare(leftArray(idx), rightArray(idx))
            if (compared != 0) {
              return compared
            }
          }
        }
        if (leftArray.size > rightArray.size) {
          1
        } else {
          0
        }
      } else {
        if (jsonLeft.isNumber) {
          this.compare(Json.arr(jsonLeft), jsonRight)
        } else {
          this.compare(jsonLeft, Json.arr(jsonRight))
        }
      }
    }
  }
}

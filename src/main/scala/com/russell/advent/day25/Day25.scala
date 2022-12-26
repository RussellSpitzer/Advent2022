package com.russell.advent.day25

import scala.collection.immutable.NumericRange

object Day25 {

  case class LRange(start: Long, end: Long) {
    def +(shift: Long): LRange = {
      copy(start + shift, end + shift)
    }

    def contains(v: Long): Boolean = {
      (start <= v) && (end >= v)
    }
  }

  object SnafuNumber {
    def snafuChar(char: Char) : Long = char match {
      case '2' => 2
      case '1' => 1
      case '0' => 0
      case '-' => -1
      case '=' => -2
    }

    def fromString(str: String) : Long = {
      var power = 1L
      var total = 0L
      for (c <- str.reverse) {
        total += snafuChar(c) * power
        power *= 5L
      }
      total
    }

    val bounds: IndexedSeq[LRange] = {
      var runningMax = 0L
      var runningMin = 0L
      (LRange(0,0)) +: (0 to 100).map { place =>
        val mag = Math.pow(5, place).toLong
        runningMin -= 2L * mag
        runningMax += 2L * mag
        LRange(runningMin, runningMax)
      }
    }

    def toSnafu(value: Long): String = {
      var comptuedValue = 0L
      var place = 0
      var output = ""

      var mag: Long = Math.pow(5, place).toLong
      while (!bounds(place).contains(value)) {
        mag = Math.pow(5, place).toLong
        place += 1
      }

      while (comptuedValue != value && place != -1) {
        mag = Math.pow(5, place).toLong
        val range = bounds(place)
        (value - comptuedValue) match {
          case v: Long if (range + 2L * mag).contains(v) =>
            output += '2'
            comptuedValue += 2L * mag
            place -= 1
          case v: Long if (range + mag).contains(v) =>
            output += '1'
            comptuedValue += mag
            place -= 1
          case v: Long if range.contains(v) =>
            output += '0'
            place -= 1
          case v: Long if (range + -1L * mag).contains(v) =>
            output += '-'
            comptuedValue -= mag
            place -= 1
          case v: Long if (range + -2L * mag).contains(v) =>
            output += '='
            comptuedValue -= 2L * mag
            place -= 1
        }
      }
      output.replaceAll("^0+","")
    }
  }
}

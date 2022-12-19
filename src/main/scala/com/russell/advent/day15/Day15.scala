package com.russell.advent.day15

import com.russell.advent.AdventUtil.Position
import com.russell.advent.AdventUtil.Position.{between, mDistance}
import com.russell.advent.util.Ranges.findContiguous

import scala.collection.mutable
import scala.io.Source

object Day15 {

  case class SensorBeacon(sensor: Position, beacon: Position) {
    val distance = mDistance(sensor, beacon)
    val minX = sensor.x - distance
    val maxX = sensor.x + distance
    val minY = sensor.y - distance
    val maxY = sensor.y + distance

    def coverageAt(y: Int): Range.Inclusive = {
      val xLeft = distance - Math.abs(sensor.y - y)
      if (xLeft > 0) {
        sensor.x - xLeft to sensor.x + xLeft
      } else {
        0 to -1
      }
    }
  }

  case class BeaconCollection(sensorBeacons: Seq[SensorBeacon]) {
    val minX = sensorBeacons.minBy(_.minX).minX
    val maxX = sensorBeacons.maxBy(_.maxX).maxX
    val minY = sensorBeacons.minBy(_.minY).minY
    val maxY = sensorBeacons.minBy(_.maxY).maxY
    val beacons = sensorBeacons.map(_.beacon).toSet

    def coverageAt(y: Int): Int = {
      val coveredSites = (minX to maxX).map(x => Position(x, y)).filter(pos => sensorsCover((pos)))
      val nonBeacon = coveredSites.filter(!beacons.contains(_))
      nonBeacon.size
    }



    def findDistressSignal(maxRange: Int = 4000000): Option[Position] = {
      var contigous = 0 to -1
      (0 to maxRange).find { y =>
        val coverages = sensorBeacons.map(_.coverageAt(y)).filter(_.nonEmpty)
        contigous = findContiguous(coverages)(0) // First contiguous range
        contigous.end < maxRange || contigous.start > 0
        }.map( y => {
        if (contigous.end < maxRange) {
          Position(contigous.end +1, y)
        } else {
          Position(contigous.start - 1, y)
        }
      })
    }

    def sensorsCover(pos: Position): Boolean = {
      sensorBeacons.exists(beacon =>
        (mDistance(beacon.sensor, pos) <= beacon.distance)
      )
    }
  }

  def parseInput(path: String): Seq[SensorBeacon] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines: Seq[String]): Seq[SensorBeacon] = {
    lines.map { line =>
      line.replace("Sensor at ", "")
        .replace(" closest beacon is at ", "")
        .replace("x=", "")
        .replace(" y=", "")
    }.map{ str =>
      val split = str.replace(":", ",").split(",")
      SensorBeacon(Position(split(0).toInt, split(1).toInt), Position(split(2).toInt, split(3).toInt))
    }
  }
}

package com.russell.advent.day15

import com.russell.advent.AdventUtil.Position
import com.russell.advent.day15.Day15._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay15 extends AnyFlatSpec with should.Matchers {

  val Example = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                  |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                  |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                  |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                  |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                  |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                  |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                  |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                  |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                  |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                  |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                  |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                  |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                  |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin.split("\n").map(_.trim)

  "Parse Input" should "correctly parse example" in {
    val beacons = parseInput(Example)
    beacons(0) should be (SensorBeacon(Position(2, 18), Position(-2, 15)))
    beacons(5) should be (SensorBeacon(Position(14, 17), Position(10, 16)))
  }

  "Determine Coverage" should "work in example" in {
    val beaconCollection: BeaconCollection = BeaconCollection(parseInput(Example))
    beaconCollection.coverageAt(10) should be (26)
  }

  it should "work in part1 input" in {
    val beaconCollection: BeaconCollection = BeaconCollection(parseInput("day15.txt"))
    beaconCollection.coverageAt(2000000) should be (6124805)
  }

  "Find distress" should "find the signal in the example" in {
    val beaconCollection: BeaconCollection = BeaconCollection(parseInput(Example))
    val pos = beaconCollection.findDistressSignal(20).get
    (pos.x * 4000000L + pos.y) should be (56000011)
  }

  it should "find the signal in input" in {
    val beaconCollection: BeaconCollection = BeaconCollection(parseInput("day15.txt"))
    val pos = beaconCollection.findDistressSignal().get
    beaconCollection.sensorsCover(pos) should be (false)
    (pos.x * 4000000L + pos.y) should be(12555527364986L)
  }
}

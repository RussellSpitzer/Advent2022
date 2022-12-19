package com.russell.advent.day18

import com.russell.advent.AdventUtil.Voxel
import com.russell.advent.day18.Day18._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable

class TestDay18 extends AnyFlatSpec with should.Matchers {

  val Example =
    """2,2,2
      |1,2,2
      |3,2,2
      |2,1,2
      |2,3,2
      |2,2,1
      |2,2,3
      |2,2,4
      |2,2,6
      |1,2,5
      |3,2,5
      |2,1,5
      |2,3,5""".stripMargin.split("\n").map(_.trim)

  "Surface area" should "be findable of the example" in {
    val voxels = parseInput(Example)
    Voxel.surfaceArea(voxels) should be (64)
  }

  it should "be findable in input" in {
    val voxels = parseInput("day18.txt")
    voxels.size should be (2817)
    Voxel.surfaceArea(voxels) should be (4390)
  }

  "Exterior surface area" should "be findable in example" in {
    val voxels = parseInput(Example)
    Voxel.externalSurfaceArea(voxels) should be (58)
  }

  it should "be finadable in input" in {
    val voxels = parseInput("day18.txt")
    Voxel.externalSurfaceArea(voxels) should be (2534)
  }
}

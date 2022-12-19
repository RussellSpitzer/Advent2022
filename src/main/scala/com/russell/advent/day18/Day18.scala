package com.russell.advent.day18

import com.russell.advent.AdventUtil.Voxel

import scala.collection.mutable
import scala.io.Source

object Day18 {
  def fillVoid(voxel: Voxel,
               startingVoxels: Set[Voxel],
               visitedVoxels: mutable.Set[Voxel],
               min: Voxel,
               max: Voxel): Boolean = {
    if (voxel.x < min.x || voxel.x > max.x ||
      voxel.y < min.y || voxel.y > max.y ||
      voxel.z < min.z || voxel.z > max.z) {
      // Found the void
      false
    } else if (startingVoxels.contains(voxel) || visitedVoxels.contains(voxel)){
      // Found an existing edge
      true
    } else {
      visitedVoxels.addOne(voxel)
      voxel.neighborVoxels.forall{ case neighbor =>
        fillVoid(neighbor, startingVoxels, visitedVoxels, min, max)
      }
    }
  }

  def parseInput(path: String): Set[Voxel] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines: Seq[String]): Set[Voxel] = {
    lines.map(_.split(",")).map( arr => Voxel(arr(0).toInt, arr(1).toInt, arr(2).toInt)).toSet
  }
}

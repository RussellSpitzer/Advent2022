package com.russell.advent.util

import scala.collection.mutable

object Voxels {
  case class Voxel(x: Int, y: Int, z: Int) {
    def neighborVoxels = Seq(
      this.copy(x = x + 1),
      this.copy(x = x - 1),
      this.copy(y = y + 1),
      this.copy(y = y - 1),
      this.copy(z = z + 1),
      this.copy(z = z - 1))

    def surfaceArea(voxels: Set[Voxel]): Int = {
      val others = (voxels - this)
      val surfaceArea = neighborVoxels.filter(!others.contains(_)).size
      surfaceArea
    }
  }

  object Voxel {
    def surfaceArea(voxels: Set[Voxel]): Int = {
      val voxelSeq = voxels.toSeq
      val surfaces = voxelSeq.map(_.surfaceArea(voxels))
      surfaces.sum
    }

    def externalSurfaceArea(input: Set[Voxel]): Int = {
      var voxels = input
      val (min, max) = Voxel.bounds(voxels)
      Voxel.voxelsInBounds(min, max).foreach { checkVoxel =>
        val visitedVoxels: mutable.Set[Voxel] = mutable.Set()
        if (fillVoid(checkVoxel, voxels, visitedVoxels, min, max)) {
          voxels = voxels ++ visitedVoxels
        }
      }
      surfaceArea(voxels)
    }

    def bounds(voxels: Set[Voxel]): (Voxel, Voxel) = {
      val max = Voxel(voxels.maxBy(_.x).x, voxels.maxBy(_.y).y, voxels.maxBy(_.z).z)
      val min = Voxel(voxels.minBy(_.x).x, voxels.minBy(_.y).y, voxels.minBy(_.z).z)
      (min, max)
    }

    def voxelsInBounds(min: Voxel, max: Voxel): Iterable[Voxel] = {
      for {x <- min.x to max.x
           y <- min.y to max.y
           z <- min.z to max.z} yield
        Voxel(x, y, z)
    }

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
      } else if (startingVoxels.contains(voxel) || visitedVoxels.contains(voxel)) {
        // Found an existing edge
        true
      } else {
        visitedVoxels.addOne(voxel)
        voxel.neighborVoxels.forall { case neighbor =>
          fillVoid(neighbor, startingVoxels, visitedVoxels, min, max)
        }
      }
    }
  }

}

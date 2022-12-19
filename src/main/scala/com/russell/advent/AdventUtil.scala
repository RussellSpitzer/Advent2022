package com.russell.advent

import scala.collection.mutable
import scala.reflect.ClassTag

object AdventUtil {

  /** Finds the left most contiguous range in a set of ranges
   **/
  def findContiguous(unorderedRanges: Seq[Range.Inclusive]): Seq[Range.Inclusive] = {
    val ranges = unorderedRanges.sortBy(_.start)
    ranges.foldLeft(Seq(): Seq[Range.Inclusive]) { (combinedRanges, range: Range.Inclusive) =>
      if (combinedRanges.isEmpty) {
        combinedRanges :+ range
      } else {
        val lastCombined = combinedRanges.last
        if (lastCombined.contains(range.start) && lastCombined.end < range.end) {
          combinedRanges.dropRight(1) :+ (lastCombined.start to range.end)
        } else {
          combinedRanges
        }
      }
    }
  }

  case class Node[T](name: String, value: T, children: Seq[Node[T]]) {
    def findDepthFirst(fun: (Node[T]) => Boolean) : Option[Node[T]] = {
      if (fun(this)) {
        Some(this)
      } else {
        children.collectFirst{case child if child.findDepthFirst(fun).isDefined => child}
      }
    }

    def toMap(): Map[String, Node[T]] = {
      val mapBuilder = Map.newBuilder[String, Node[T]]
      mapBuilder.addOne(name, this)
      mapBuilder.addAll(children.map(_.toMap()).reduce(_ ++ _))
      mapBuilder.result()
    }
  }

  case class Position(x: Int, y: Int) {
    def up() = copy(y = y + 1)
    def down(): Position = copy(y = y - 1)
    def left(): Position = copy(x = x - 1)
    def right(): Position = copy(x = x + 1)
    def adjacent(): Seq[Position]  = Seq(up, down, left, right)
    def diagonals(): Seq[Position] = Seq(up.right, up.left, down.left, down.right)

    def +(that: Position) = {
      Position(x + that.x, y + that.y)
    }

  }
  object Position {
    val Origin = Position(0, 0)

    /**
     * All the points that lie on a straight line between this point
     * and that point inclusive
     */
    def between(start: Position, end: Position): Seq[Position] = {
      val dx = end.x - start.x
      val dy = end.y - start.y
      assert(dx * dy == 0 && (dx != 0 || dy != 0), s"No straight line between points ($start -> $end)")
      if (dx != 0) {
        for (shift <- 0 to dx by dx.sign) yield
          start.copy(x = start.x + shift)
      } else {
        for (shift <- 0 to dy by dy.sign) yield
          start.copy(y = start.y + shift)
      }
    }

    def mDistance(start: Position, end: Position): Int = {
      val dx = end.x - start.x
      val dy = end.y - start.y
      Math.abs(dx) + Math.abs(dy)
    }
  }

  case class XYArray[T](data: Array[Array[T]]) extends mutable.IndexedSeq[Array[T]] {

    val dimx = data(0).size
    val dimy = data.size
    def inBounds(position: Position): Boolean = {
      position.x >= 0 && position.x < dimx && position.y >= 0 && position.y < dimy
    }

    def apply(position: Position): T = {
      data(position.y.toInt)(position.x.toInt)
    }

    def apply(i: Int): Array[T] = {
      data(i)
    }

    def update(position: Position, value: Any): Unit = {
      data(position.y.toInt)(position.x.toInt) = value.asInstanceOf[T]
    }

    override def length: Int = dimy

    override def update(idx: Int, elem: Array[T]): Unit =
      data(idx) = elem
  }

  object XYArray {
    def apply[T: ClassTag](dimx: Int, dimy: Int): XYArray[T] = {
      new XYArray[T](Array.ofDim(dimy, dimx))
    }

    def apply[T: ClassTag](seqs: Seq[Seq[T]]): XYArray[T] = {
      XYArray[T](seqs.map(_.toArray).toArray)
    }
  }

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
  }

}

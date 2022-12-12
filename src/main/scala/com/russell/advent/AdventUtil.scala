package com.russell.advent

import scala.collection.mutable
import scala.reflect.ClassTag

object AdventUtil {

  case class Position(x: Int, y: Int) {
    def up() = copy(y = y + 1)
    def down(): Position = copy(y = y - 1)
    def left(): Position = copy(x = x - 1)
    def right(): Position = copy(x = x + 1)
    def adjacent(): Seq[Position]  = Seq(up, down, left, right)
    def diagonals(): Seq[Position] = Seq(up.right, up.left, down.left, down.right)
  }
  object Position {
    val Origin = Position(0, 0)
  }

  case class XYArray[T](data: Array[Array[T]]) extends mutable.IndexedSeq[Array[T]] {

    val dimx = data(0).size
    val dimy = data.size
    def inBounds(position: Position): Boolean = {
      position.x >= 0 && position.x < dimx && position.y >= 0 && position.y < dimy
    }

    def apply(position: Position): T = {
      data(position.y)(position.x)
    }

    def apply(i: Int): Array[T] = {
      data(i)
    }

    def update(position: Position, value: Any): Unit = {
      data(position.y)(position.x) = value.asInstanceOf[T]
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

}

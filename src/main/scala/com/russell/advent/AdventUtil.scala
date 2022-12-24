package com.russell.advent

import scala.collection.mutable
import scala.reflect.ClassTag

object AdventUtil {

  object Direction extends Enumeration {
    type Direction = Value
    val N, NE, NW, S, SE, SW, E, W = Value
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

    def N() = copy(y = y - 1)
    def NE() = copy(x = x + 1, y = y - 1)
    def NW() = copy(x = x -1, y = y - 1)
    def S() = copy(y = y + 1)
    def SE() = copy(x = x + 1, y = y + 1)
    def SW() = copy(x = x - 1, y = y + 1)
    def E() = copy(x = x + 1)
    def W() = copy(x = x - 1)
    def down() = S
    def up() = N
    def left() = W
    def right() = E
    def adjacent(): Seq[Position]  = Seq(N, S, E, W)
    def diagonals(): Seq[Position] = Seq(N, NE, E, SE, S, SW, W, NW)

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
}

package com.russell.advent.day8

import scala.io.Source

object Day8 {

  def parseTrees(path: String): IndexedSeq[IndexedSeq[Int]] = {
    parseTrees(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseTrees(lines: Seq[String]): IndexedSeq[IndexedSeq[Int]] = {
    lines.map(
      row => row.map( s => s.toString.toInt).toIndexedSeq
    ).toIndexedSeq
  }

  def treesVisible(trees: IndexedSeq[IndexedSeq[Int]]) : Seq[(Int, Int)] = {
    (0 until trees.length)
      .flatMap (x => (0 until trees(0).length)
      .map(y => (x, y)))
      .filter{case (x, y) => treeVisible(x, y, trees)}
  }

  def treeVisible(x: Int, y: Int, trees: IndexedSeq[IndexedSeq[Int]]): Boolean = {
    val treeHeight = trees(x)(y)
    val x_max = trees.length
    val y_max = trees(x).length
    (0 until x).forall( i => trees(i)(y) < treeHeight) ||
    (x + 1 until x_max).forall( i => trees(i)(y) < treeHeight) ||
      (0 until y).forall(i => trees(x)(i) < treeHeight) ||
      (y + 1 until y_max).forall(i => trees(x)(i) < treeHeight)
  }

  def viewFrom(x: Int, y: Int, trees: IndexedSeq[IndexedSeq[Int]]) : (Int, Int, Int, Int) = {
    val treeHeight = trees(x)(y)

    var blocked = false
    val rightView = trees(x).slice(y + 1, trees(x).length)
      .takeWhile{ newHeight =>
        if (blocked) {
          false
        } else {
          blocked = newHeight >= treeHeight
          true
        }
      }

    blocked = false
    val leftView = trees(x).slice(0, y).reverse.takeWhile { newHeight =>
      if (blocked) {
        false
      } else {
        blocked = newHeight >= treeHeight
        true
      }
    }

    val invertedTrees = trees.transpose
    blocked = false
    val downView = invertedTrees(y).slice(x + 1, invertedTrees(y).length)
      .takeWhile{ newHeight =>
        if (blocked) {
          false
        } else {
          blocked = newHeight >= treeHeight
          true
        }
      }

    blocked = false
    val upView = invertedTrees(y).slice(0, x).reverse.takeWhile { newHeight =>
      if (blocked) {
        false
      } else {
        blocked = newHeight >= treeHeight
        true
      }
    }

    (leftView.size, rightView.size, upView.size, downView.size)
  }

  def scenicScore(trees: IndexedSeq[IndexedSeq[Int]]): Int = {
    (0 until trees.length)
      .flatMap(x => (0 until trees(0).length).map(y => (x, y)))
      .map{ case (x, y) =>
        val views = viewFrom(x, y, trees)
        val score = views._1 * views._2 * views._3 * views._4
        score
      }.max
  }
}

package com.russell.advent.day18

import com.russell.advent.util.Voxels.Voxel

import scala.io.Source

object Day18 {


  def parseInput(path: String): Set[Voxel] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines: Seq[String]): Set[Voxel] = {
    lines.map(_.split(",")).map( arr => Voxel(arr(0).toInt, arr(1).toInt, arr(2).toInt)).toSet
  }
}

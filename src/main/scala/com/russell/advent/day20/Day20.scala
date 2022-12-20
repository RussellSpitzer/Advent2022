package com.russell.advent.day20

import scala.io.Source

object Day20 {

  def parseInput(path: String): Array[Long] = {
    parseInput(Source.fromResource(path).getLines.map(_.trim).toSeq)
  }

  def parseInput(input: Seq[String]): Array[Long] = {
    input.map(_.toLong).toArray
  }

  def mixedElement(file: Array[Long], idx: Int): Long = {
    file((file.indexOf(0) + idx) % file.size)
  }

  def mix(file: Array[Long], numMix: Int= 1): Array[Long] = {
    val mixOrder = file.zipWithIndex.clone().toSeq
    val workingBuffer = file.zipWithIndex.toBuffer
    val size: Long = file.size - 1
    for (i <- 1 to numMix) {
      for ((element, idx) <- mixOrder if element != 0 && element % (size) != 0) {
        val distance = element % size
        val origin = workingBuffer.indexOf((element, idx))
        workingBuffer.remove(origin)
        var target = ((distance + origin) % size).toInt
        if (target < 0) {
          target = (size + (distance + origin)).toInt
        }
        //println(s"$element moves between ${workingBuffer(if (target != 0) target - 1 else size -1)} and ${workingBuffer((target) % size)}")
        workingBuffer.insert(target, (element, idx))
      }
    }
    workingBuffer.map(_._1).toArray
  }

}

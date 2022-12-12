package com.russell.advent.day5

import scala.collection.mutable
import scala.io.Source

object Day5 {

  case class Move(number: Int, from: Int, to: Int)

  def parseInput(path: String): (IndexedSeq[mutable.Stack[Char]], Seq[Move])= {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines : Seq[String]): (IndexedSeq[mutable.Stack[Char]], Seq[Move]) = {

    val stacks = (1 to 9).map(_ => new mutable.Stack[Char])
    val moves = List.newBuilder[Move]

    for (line <- lines) {
      if (line.contains("[")) {
        val entries = line.grouped(4)
        for ((entry, index) <- entries.zipWithIndex if entry(1) != 32) {
          stacks(index).append(entry(1))
        }
      } else if (line.startsWith("move")) {
        val strMove = line
          .replace("move ", "")
          .replace("from ", "")
          .replace("to ", "")
          .split(" ")
          .toSeq
          .map(_.toInt)
        moves.addOne(Move(strMove(0), strMove(1) - 1, strMove(2) - 1))
      }
    }
    (stacks, moves.result())
  }

  def applyMove(stacks: Seq[mutable.Stack[Char]], move: Move)  = {
    val toBeMoved = (1 to move.number)
      .map(_ => stacks(move.from).pop())
    stacks(move.to).pushAll(toBeMoved)
  }

  def applyPart2Move(stacks: Seq[mutable.Stack[Char]], move: Move) = {
    val toBeMoved = (1 to move.number)
      .map(_ => stacks(move.from).pop()).reverse
    stacks(move.to).pushAll(toBeMoved)
  }

}

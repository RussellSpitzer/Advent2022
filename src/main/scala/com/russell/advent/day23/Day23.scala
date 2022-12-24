package com.russell.advent.day23

import com.russell.advent.AdventUtil.Position

import scala.io.Source

object Day23 {

  def parseInput(path: String): Set[Position] = {
    parseInput(Source.fromResource(path).getLines.toSeq)
  }
  def parseInput(lines: Seq[String]): Set[Position] = {
    lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.collect { case (c, x) if c == '#' =>
        Position(x, y)
      }
    }.toSet
  }

  def move(elves: Set[Position], order: Seq[Int]): Set[Position] = {
    // println(s"\nBeginning move - Considering $order")
    val proposedMoves = Map.newBuilder[Position, Position]
    for (elf <- elves) {

      val NW = elf.up().left()
      val N = elf.up()
      val NE = elf.up().right()
      val E =  elf.right()
      val SE = elf.down().right()
      val S = elf.down()
      val SW = elf.down().left()
      val W = elf.left()

      // println(debugString(elf, elves))


      val emptyMoves = order.filter { proposalId =>
        proposalId == 0 && Seq(N, NE, NW).forall(adj => !elves.contains(adj)) ||
          proposalId == 1 && Seq(S, SE, SW).forall(adj => !elves.contains(adj)) ||
          proposalId == 2 && Seq(W, NW, SW).forall(adj => !elves.contains(adj)) ||
          proposalId == 3 && Seq(E, NE, SE).forall(adj => !elves.contains(adj))
      }

      if (emptyMoves.size == 0 || emptyMoves.size == 4) {
        // println(s"No Move $elf")
      } else {
        emptyMoves(0) match {
          case 0 =>
            // println (s"North Move $elf => $N")
            proposedMoves.addOne(elf, N)
          case 1 =>
            // println (s"South Move $elf => $S")
            proposedMoves.addOne(elf, S)
          case 2 =>
            // println (s"West Move $elf => $W")
            proposedMoves.addOne(elf, W)
          case 3 =>
            // println (s"East Move $elf => $E")
            proposedMoves.addOne(elf, E)
        }
      }
    }

    // Remove duplicate moves
    val maybeMove = proposedMoves.result()
    val moveFiled = maybeMove.groupBy(_._2).map(_._2).filter(_.values.size > 1)
    // println(moveFiled)


    val nonMovers = moveFiled.map(_.keys).reduceOption(_ ++ _).getOrElse(Set.empty)
    // println (nonMovers)
    val validMoves = maybeMove -- nonMovers

    // Move
    val realMoves = elves.map(elf => validMoves.getOrElse(elf, elf))

    // println(s"${maybeMove.size} potentital moves - ${nonMovers.size} non movers = ${validMoves.size}")
    realMoves
  }

  def emptyTiles(positions: Set[Position]): Int = {
    val minX = positions.minBy(_.x).x
    val maxX = positions.maxBy(_.x).x
    val minY = positions.minBy(_.y).y
    val maxY = positions.maxBy(_.y).y

    (minY to maxY).size * (minX to maxX).size - positions.size
  }

  def rotateChoices(choices: Seq[Int]): Seq[Int] = {
    choices.tail :+ choices.head
  }

  def debugString(elf: Position, elves: Set[Position]): String = {
    val minX = elf.x - 1
    val maxX = elf.x + 1
    val minY = elf.y - 1
    val maxY = elf.y + 1

    (minY to maxY).map(y =>
      (minX to maxX).map(x => {
        val p = Position(x, y)
        if (p == elf) {
          "O"
        } else if (elves.contains(p)) {
          "#"
        } else {
          "."
        }
      }).mkString("")
    ).mkString("\n")
  }

  def elvesString(positions: Set[Position]): String = {
    val minX = positions.minBy(_.x).x
    val maxX = positions.maxBy(_.x).x
    val minY = positions.minBy(_.y).y
    val maxY = positions.maxBy(_.y).y

    (minY to maxY).map(y =>
      (minX to maxX).map(x => if (positions.contains(Position(x, y))) "#" else ".").mkString("")
    ).mkString("\n") + "\n"
  }
}

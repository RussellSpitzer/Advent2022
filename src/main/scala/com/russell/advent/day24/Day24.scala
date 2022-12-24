package com.russell.advent.day24

import com.russell.advent.AdventUtil.Direction._
import com.russell.advent.AdventUtil.{Direction, Position}

import scala.collection.mutable
import scala.io.Source

object Day24 {

  def parseInput(path: String): World = {
    parseInput(Source.fromResource(path).getLines.toSeq)
  }

  def parseInput(lines: Seq[String]): World = {
    val blizzardBuilder = Set.newBuilder[Blizzard]
    val maxX = lines(0).size - 2
    val maxY = lines.size - 2
    lines.zipWithIndex.foreach { case (line, y) => line.zipWithIndex.foreach { case (character, x) =>
      Direction.Arrow.get(character).foreach(direction =>
        blizzardBuilder.addOne(Blizzard(Position(x - 1, y - 1), direction)))
    }}

    World(blizzardBuilder.result(), maxX, maxY)
  }


  case class State(pos: Position, time: Int)
  case class Blizzard(position: Position, direction: Direction)

  case class World(initialBlizzards: Set[Blizzard], maxX: Int, maxY: Int) {
    val start = State(Position(0, -1), 0)
    val end = Position(maxX - 1, maxY)

    val BlizzardCache: mutable.Map[Int, Set[Position]] = mutable.Map()
    val statesProcessed: mutable.Set[State] = mutable.Set()

    def blizzardsAtTime(time: Int): Set[Position] = {
      BlizzardCache.getOrElseUpdate(time,
        initialBlizzards.map { blizzard =>
          blizzard.direction match {
            case N => blizzard.position.copy(y = (((blizzard.position.y - time) % maxY) + maxY) % maxY)
            case S => blizzard.position.copy(y = (((blizzard.position.y + time) % maxY) + maxY) % maxY)
            case E => blizzard.position.copy(x = (((blizzard.position.x + time) % maxX) + maxX) % maxX)
            case W => blizzard.position.copy(x = (((blizzard.position.x - time) % maxX) + maxX) % maxX)
          }
        }
      )
    }

    def moveThroughBlizzards(firstSpot: Position = start.pos, targetSpot: Position = end, time: Int = 0): Int = {
      val statesToProcess: mutable.Queue[State] = mutable.Queue()

      var finalState: Option[State] = None

      statesProcessed.add(State(firstSpot, time))
      statesToProcess.enqueue(State(firstSpot, time))

      while (statesToProcess.nonEmpty && finalState.isEmpty) {
        val s = statesToProcess.dequeue()
        val blizzardPositions = blizzardsAtTime(s.time + 1)

        for (adj <- s.pos.adjacent() :+ s.pos) {
          if (adj == targetSpot) {
            finalState = Some(s.copy(adj, s.time + 1))
          } else if (adj == firstSpot) {
            statesProcessed.add(State(firstSpot, s.time + 1))
            statesToProcess.enqueue(State(firstSpot, s.time + 1))
          } else if (adj.x >= maxX || adj.x < 0 || adj.y >= maxY || adj.y < 0){
            // Wall
          } else if (!blizzardPositions.contains(adj)) {
            val nextState = s.copy(adj, s.time + 1)
            if (!statesProcessed.contains(nextState)) {
              statesProcessed.add(nextState)
              statesToProcess.enqueue(nextState)
            }
          }
        }
      }
      finalState.map(_.time).getOrElse(-1)
    }

    def display(lastTime: Int = 3): Unit = {
      for (time <- 0 to lastTime) {
        println(s"Time = $time")
        val blizzards = blizzardsAtTime(time)
        println("#."+Array.fill(maxX)("#").mkString(""))
        for (y <- 0 until maxY) {
          println("#" + (0 until maxX).map{ x =>
            if (blizzards.contains(Position(x, y))) {
              'X'
            } else if (statesProcessed.contains(State(Position(x, y), time))) {
              'O'
            } else {
              '.'
            }
          }.mkString("") + "#")
        }
        println(Array.fill(maxX)("#").mkString("") + ".#" + "\n")
      }
    }
  }



}

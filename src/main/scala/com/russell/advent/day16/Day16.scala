package com.russell.advent.day16

import com.russell.advent.AdventUtil.Node

import scala.collection.mutable
import scala.io.Source
import scala.reflect.ClassTag

object Day16 {

  case class MapBackedDAG[T](values: Map[String, T], connections: Map[String, Seq[String]]){

    def nodesByDepth(root: String): Seq[(String, Int)] = {
      val toVisit: mutable.Queue[(String, Int)] = mutable.Queue()
      val visited: mutable.Set[String] = mutable.Set()
      var result: Seq[(String, Int)] = Seq.empty

      toVisit.enqueue((root, 0))
      visited.add(root)
      result = result :+ ((root, 0))
      while (toVisit.nonEmpty) {
        val (node, depth) = toVisit.dequeue()
        connections(node).filter(!visited.contains(_)).foreach { child =>
          toVisit.enqueue((child, depth +1))
          visited.add(child)
          result = result :+ (child, depth +1)
        }
      }
      result
    }

    def breadthFirstTraverse[V: ClassTag](root: String,
                                          fun: (String, Int, T) => V): Map[String, V] = {
      nodesByDepth(root).map { case (node, depth) =>
        (node, fun(node, depth, values(node)))
      }.toMap
    }
  }


  case class ValveOutput (name: String, timeConsumed: Int, score: Int)
  def valveValue(node: String, time: Int, value: Int, timeLimit: Int = 30): ValveOutput = {
    ValveOutput(node, time + 1, (timeLimit - (time + 1)) * value)
  }

  def parseInput(path: String): MapBackedDAG[Int] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines: Seq[String]): MapBackedDAG[Int] = {
    val valueMapBuilder = Map.newBuilder[String, Int]
    val tunnelMapBuilder = Map.newBuilder[String, Seq[String]]
    lines.foreach { line =>
      val s = line
        .replace("Valve ", "")
        .replace(" has flow rate=", ",")
        .replaceAll(" tunnel[s]? lead[s]? to valve[s]? ", "")
        .split(";")
      val name = s(0).split(",")(0)
      val value = s(0).split( ",")(1).toInt
      val tunnels = s(1).split(", ")
      valueMapBuilder.addOne(name, value)
      tunnelMapBuilder.addOne(name, tunnels)
    }
    val valueMap = valueMapBuilder.result()
    val tunnelMap = tunnelMapBuilder.result()
    MapBackedDAG(valueMap, tunnelMap)
  }

  def bestPath(tunnels: MapBackedDAG[Int],
               start: String = "AA",
               visited: Set[String] = Set.empty[String],
               timeLeft: Int = 30): Seq[Seq[ValveOutput]] = {
    tunnels
      .breadthFirstTraverse(start, valveValue(_, _, _, timeLeft))
      .filter{ case (node, output) => !visited.contains(node) && output.score > 0 && output.timeConsumed < timeLeft}
      .flatMap{ case (node, output) =>
        val children = bestPath(
          tunnels,
          node,
          visited + node,
          timeLeft - output.timeConsumed)
        Seq(output) +: children.map(output +: _)
      }.toSeq
  }
}

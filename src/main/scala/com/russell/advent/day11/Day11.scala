package com.russell.advent.day11

import scala.io.Source

object Day11 {

  case class Monkey (var items: List[Long],
                     operation: (Long) => (Long),
                     divisibleBy: Long,
                     destination: (Int, Int),
                     var numInspection: Long = 0) {
    def test(i: Long) = i % divisibleBy == 0
  }

  def parseInput(path: String): IndexedSeq[Monkey] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(line: Seq[String]): IndexedSeq[Monkey] = {
    line.filterNot(_.isBlank).grouped(6).map {case monkeyString =>
      Monkey(
        monkeyString(1).split(": ")(1).split(", ").map(_.toLong).toList,
        operation = parseOperation(monkeyString(2)),
        monkeyString(3).split(": ")(1).split(" ")(2).toLong,
        (monkeyString(4).split(" ")(5).toInt, monkeyString(5).split(" ")(5).toInt)
      )
    } .toIndexedSeq
  }

  def parseOperation(opString: String) = {
    val opArgs = opString.split(" = ")(1).split(" ").map(_.trim)
    (old: Long) => {
      val arg1 = if (opArgs(0).equals("old")) old else opArgs(0).toLong
      val arg2 = if (opArgs(2).equals("old")) old else opArgs(2).toLong
      opArgs(1) match {
        case "+" => arg1 + arg2
        case "*" => arg1 * arg2
        case "/" => arg1 / arg2
        case "-" => arg1 - arg2
      }
    }
  }

  def runRound(monkeys: Seq[Monkey]) : Seq[Long] = {
    for (monkey <- monkeys) yield {
      val itemsToToss = monkey.items
      monkey.items = List()
      for (item <- itemsToToss) {
        val newWorry = monkey.operation(item) / 3
        if (monkey.test(newWorry)) {
          monkeys(monkey.destination._1).items = monkeys(monkey.destination._1).items :+ newWorry
        } else {
          monkeys(monkey.destination._2).items = monkeys(monkey.destination._2).items :+ newWorry
        }
      }
      monkey.numInspection += itemsToToss.length
      itemsToToss.length
    }
  }

  def runReallyWorryRound(monkeys: Seq[Monkey]): Seq[Long] = {

    val commonWorry = monkeys.map(_.divisibleBy).reduce(_ * _)
    for (monkey <- monkeys) yield {
      val itemsToToss = monkey.items
      monkey.items = List()
      for (item <- itemsToToss) {
        val newWorry = monkey.operation(item) % commonWorry
        if (monkey.test(newWorry)) {
          monkeys(monkey.destination._1).items = monkeys(monkey.destination._1).items :+ newWorry
        } else {
          monkeys(monkey.destination._2).items = monkeys(monkey.destination._2).items :+ newWorry
        }
      }
      monkey.numInspection += itemsToToss.length
      itemsToToss.length
    }
  }

  def scoreMonkeys(monkeys: Seq[Monkey]) : Long = {
    monkeys.sortBy(_.numInspection).reverse.take(2).map(_.numInspection).reduce(_ * _)
  }
}

package com.russell.advent.day12

import com.russell.advent.AdventUtil.Position
import com.russell.advent.AdventUtil.Position._
import com.russell.advent.day12.Day12._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class TestDay12 extends AnyFlatSpec with should.Matchers {

  val Example = """Sabqponm
                 |abcryxxl
                 |accszExk
                 |acctuvwj
                 |abdefghi""".stripMargin.split("\n").map(_.trim)

  "ParseInput" should "work on the example" in {
    val parsed = parseInput(Example)
    parsed.start should be (Origin)
    parsed.end should be (Position(5, 2))
  }

  it should "work on input" in {
    val parsed = parseInput("day12.txt")
    parsed.start should be(Position(0, 20))
    parsed.end should be(Position(136, 20))
  }

  "FindMinCost" should "work on the example" in {
    val parsed = parseInput(Example)
    val result = findMinPathToStart(parsed)
    result.foreach(s => println(s.mkString(" ")))
    result(0)(0) should be (31)
  }

  it should "work on part1" in {
    val parsed = parseInput("day12.txt")
    val result = findMinPathToStart(parsed)
    result.foreach(s => println(s.mkString(" ")))
    result(parsed.start.y)(parsed.start.x) should be(504)
  }

  it should "work on part2 example" in {
    val parsed = parseInput(Example)
    val result = findMinPathToStart(parsed)
    val starts = for {
      (row, y) <- parsed.board.zipWithIndex
      (value, x) <- row.zipWithIndex if value == 0
    } yield {
      Position(x, y)
    }
    starts.map(p => result(p.y)(p.x)).min should be (29)
  }

  it should "work for part2" in {
    val parsed = parseInput("day12.txt")
    val result = findMinPathToStart(parsed)
    val starts = for {
      (row, y) <- parsed.board.zipWithIndex
      (value, x) <- row.zipWithIndex if value == 0
    } yield {
      Position(x,y)
    }
    starts.map(p => result(p.y)(p.x)).filter(_ != 0).min should be (500)
  }

}
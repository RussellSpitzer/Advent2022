package com.russell.advent

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay1 extends AnyFlatSpec with should.Matchers {


  "Parsing" should "correctly parse a file" in {
    val elves = day1.Day1.parseInput("day1.txt")
    elves.size should be > 0
    elves.forall{case (_, cals) => cals.length > 0} shouldBe true
  }

  "The max elf" should "be findable" in {
    val elves = day1.Day1.parseInput("day1.txt")
    val max = elves.map{ case (index, cals) => (index, cals.sum)}.maxBy(_._2)
    elves.forall{ case (_ , cals) => cals.sum <= max._2} shouldBe true
    println(max)
  }

  "The top 3 elves" should "be findable" in {
    val elves = day1.Day1.parseInput("day1.txt")
    val ordered = elves.map { case (index, cals) => (index, cals.sum) }.toSeq.sortBy(-_._2)
    val top = ordered.slice(0,3)
    top should have size 3
    ordered.slice(3, ordered.size - 3).forall{ case (_, cals) => cals <= (top(2)._2)} shouldBe true
    println(top.map(_._2).sum)
  }
}

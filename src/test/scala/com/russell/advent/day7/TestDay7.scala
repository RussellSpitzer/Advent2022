package com.russell.advent.day7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Day7._

class TestDay7 extends AnyFlatSpec with should.Matchers {

  val Examples =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin.split("\n")

  "Example" should "work in part 1" in {
    val (dirs, entrySizes) = parseDirs(Examples)
    entrySizes.keys.size should be(14)
    entrySizes(Seq("/")) should be(48381165)
    entrySizes(Seq("/", "a", "e")) should be(584)
    entrySizes(Seq("/", "a")) should be(94853)

    entrySizes.filter { case (_, size) => size > 1 }
    dirs.map(dir => entrySizes(dir)).filter(_ <= 100000).sum should be(95437)
  }

  it should "work in part 2" in {
    val (dirs, entrySizes) = parseDirs(Examples)
    val totalUnused = 70000000 - entrySizes(Seq("/"))
    dirs.map(dir => entrySizes(dir)).sorted.find(_ >= (30000000 - totalUnused)).get should be(24933642)
  }

  "Part1" should "work" in {
    val (dirs, entrySizes) = parseDirs("day7.txt")
    println(dirs.map(dir => entrySizes(dir)).filter(_ <= 100000).sum)
  }

  "Part2" should "work" in {
    val (dirs, entrySizes) = parseDirs("day7.txt")
    val totalUnused = 70000000 - entrySizes(Seq("/"))
    println(totalUnused)
    println(dirs.map(dir => entrySizes(dir)).sorted.find(_ >= (30000000 - totalUnused)).get)
  }



}

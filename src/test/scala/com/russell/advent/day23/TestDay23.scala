package com.russell.advent.day23


import com.russell.advent.AdventUtil.Position
import com.russell.advent.day23.Day23._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay23 extends AnyFlatSpec with should.Matchers {

  val Example = """....#..
                  |..###.#
                  |#...#.#
                  |.#...##
                  |#.###..
                  |##.#.##
                  |.#..#..""".stripMargin.split("\n")

  val SmallExample = """.....
                       |..##.
                       |..#..
                       |.....
                       |..##.
                       |.....""".stripMargin.split("\n")


  val DenseExample =
    """.....
      |.#####.
      |.#####.
      |.#####.
      |.#####.
      |.#####.""".stripMargin.split("\n")


  "Parse" should "parse the example" in {
    val elves  = parseInput(Example)
    println(elvesString(elves))
    elves should contain (Position(0,2))
  }

  "Move" should "work for small example" in {
    var elves  = parseInput(SmallExample)
    println(elvesString(elves))
    elves = move(elves, Seq(0, 1, 2 ,3))
    println(elvesString(elves))
    elves = move(elves, Seq(1, 2, 3, 0))
    println(elvesString(elves))
    elves = move(elves, Seq(2, 3, 1, 0))
    println(elvesString(elves))
  }

  it should "work on the big example" in {
    var elves  = parseInput(Example)
    var order = Seq(0,1,2,3)
    for (i <- 1 to 10) {
      elves = move(elves, order)
      order = rotateChoices(order)
    }
    println(elvesString(elves))
    emptyTiles((elves)) should be (110)
  }

  it should "work on the dense example" in {
    var elves = parseInput(DenseExample)
    var order = Seq(0, 1, 2, 3)
    elves.foreach(elf => elf.y should be >= 0)
    elves.foreach(elf => elf.x should be >= 0)
    for (i <- 1 to 10) {
      val oldSize = elves.size
      elves = move(elves, order)
      order = rotateChoices(order)
      val newSize = elves.size
      oldSize should be(newSize)
    }
    println(elvesString(elves))
    emptyTiles((elves)) should be(131)
  }

  it should "work on the input" in {
    var elves = parseInput("day23.txt")
    var order = Seq(0,1,2,3)
    elves.foreach(elf => elf.y should be >= 0)
    elves.foreach(elf => elf.x should be >= 0)
    for (i <- 1 to 10) {
      val oldSize = elves.size
      elves = move(elves, order)
      order = rotateChoices(order)
      val newSize = elves.size
      oldSize should be (newSize)
    }
    emptyTiles((elves)) should be(4000)
  }

  it should "run until no moves" in {
    var elves = parseInput(Example)
    var oldPosition = elves
    var order = Seq(0, 1, 2, 3)
    var moves = 0
    do {
      moves += 1
      oldPosition = elves
      elves = move(elves, order)
      order = rotateChoices(order)
    } while (elves != oldPosition)

    moves should be (20)
  }

  it should "run until no moves in the input" in {
    var elves = parseInput("day23.txt")
    var oldPosition = elves
    var order = Seq(0, 1, 2, 3)
    var moves = 0
    do {
      moves += 1
      oldPosition = elves
      elves = move(elves, order)
      order = rotateChoices(order)
    } while (elves != oldPosition)

    moves should be(1040)
  }



}

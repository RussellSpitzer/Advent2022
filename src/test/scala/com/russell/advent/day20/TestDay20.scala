package com.russell.advent.day20

import com.russell.advent.day20.Day20._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestDay20 extends AnyFlatSpec with should.Matchers {

  val Example = """1
                  |2
                  |-3
                  |3
                  |-2
                  |0
                  |4""".stripMargin.split("\n").map(_.trim)

  "Mix" should "work with example" in {
    val mixed = mix(parseInput(Example))
    mixed should contain theSameElementsInOrderAs(Seq(-2,1,2,-3,4,0,3))
  }

  it should "work with other examples" in {
    mix(Array(0, -1000)) should contain theSameElementsInOrderAs(Array(0, -1000))
    mix(Array(1,2, 3, -10))  should contain theSameElementsInOrderAs(Array(1,3,-10, 2))
  }

  it should "work with part2 example" in {
    val mixed = mix(parseInput(Example).map(_ * 811589153L), 10)
    mixedElement(mixed, 1000) should be(811589153L)
    mixedElement(mixed, 2000) should be(2434767459L)
    mixedElement(mixed, 3000) should be(-1623178306L)
  }

  "MixedElement" should "work with example" in {
    val mixed = mix(parseInput(Example))
    (0 to 12).map(mixedElement(mixed, _))should contain theSameElementsInOrderAs (
      Seq(0,3,-2,1,2,-3,4,0,3,-2,1,2, -3)
    )
    mixedElement(mixed, 999) should be (-3)
    mixedElement(mixed, 1000) should be (4)
    mixedElement(mixed, 2000) should be (-3)
    mixedElement(mixed, 3000) should be (2)
  }

  it should "work with input" in {
    val mixed = mix(parseInput(("day20.txt")))
    //2307 and -4962 are wrong
    val valuesToFind = (1000 to 3000 by 1000).map(mixedElement(mixed, _))
    valuesToFind should be (Seq(4706, 1612, 4755))
    valuesToFind.sum should be (11073)
  }

  it should "work with part2 input" in {
    val mixed = mix(parseInput("day20.txt").map(_ * 811589153L), 10)
    val valuesToFind = (1000 to 3000 by 1000).map(mixedElement(mixed, _))
    valuesToFind should be(Seq(4309538402430L, 1865031873594L, 4927969337016L))
    valuesToFind.sum should be(11102539613040L)
  }

}

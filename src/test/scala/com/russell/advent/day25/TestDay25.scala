package com.russell.advent.day25

import com.russell.advent.day25.Day25._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source

class TestDay25 extends AnyFlatSpec with should.Matchers {

  val Example = """1=-0-2
                  |12111
                  |2=0=
                  |21
                  |2=01
                  |111
                  |20012
                  |112
                  |1=-1=
                  |1-12
                  |12
                  |1=
                  |122""".stripMargin.split("\n")

  val ConvertedExamples = """1747
                            |906
                            |198
                            |11
                            |201
                            |31
                            |1257
                            |32
                            |353
                            |107
                            |7
                            |3
                            |37""".stripMargin.split("\n").map(_.toLong)


  "Snafu" should "convert examples to int" in {
    Example.map(SnafuNumber.fromString) should contain theSameElementsInOrderAs (ConvertedExamples)
    Example.map(SnafuNumber.fromString).sum should be (4890)
  }

  it should "convert examples back to Snafu" in {
    ConvertedExamples.map(SnafuNumber.toSnafu) should contain theSameElementsAs (Example)
  }

  it should "sum the example and convert it to Snafu" in {
    val decimalSum = Source.fromResource("day25.txt").getLines().map(SnafuNumber.fromString).sum
    val snafuSum = SnafuNumber.toSnafu(decimalSum)

    SnafuNumber.fromString(snafuSum) should be (decimalSum)
    snafuSum should be ("20=02=120-=-2110-0=1")
  }
}

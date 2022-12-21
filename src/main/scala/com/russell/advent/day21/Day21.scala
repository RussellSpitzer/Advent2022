package com.russell.advent.day21

import scala.collection.mutable
import scala.io.Source

object Day21 {
  def parseInput(path: String): MonkeyGroup = {
    parseInput(Source.fromResource(path).getLines.map(_.trim).toSeq)
  }

  def parseInput(input: Seq[String]): MonkeyGroup = {
    val vars = Map.newBuilder[String, Long]
    val exprs = Map.newBuilder[String, Expression]
    input.map{ line: String =>
      val arr = line.split(": ")
      (arr(0), arr(1).split(" "))
    }.foreach{
      case arr if arr._2.length == 1 =>
        vars.addOne(arr._1, arr._2(0).toLong)
      case arr if arr._2.length == 3 =>
        exprs.addOne(arr._1, Expression(arr._1, arr._2(0), arr._2(2), arr._2(1)(0)))
    }
    MonkeyGroup(vars.result(), exprs.result())
  }

  case class Expression(resultName: String, arg1Name: String, arg2Name: String, operator: Char)

  case class MonkeyGroup (origVar: Map[String, Long], origExp: Map[String, Expression]) {
    def solve(expression: Expression): Boolean = expression match {
      case Expression(resultName, arg1Name, arg2Name, operator) =>
        if (!variables.contains(resultName) &&
          variables.contains(arg1Name) &&
          variables.contains(arg2Name)) {

          val (arg1, arg2) = (variables(arg1Name), variables(arg2Name))
          operator match {
            case '*' => variables(resultName) = arg1 * arg2
            case '/' => variables(resultName) = arg1 / arg2
            case '-' => variables(resultName) = arg1 - arg2
            case '+' => variables(resultName) = arg1 + arg2
          }
          //println(s"$arg1Name ($arg1) $operator $arg2Name ($arg2) = $resultName (${variables(resultName)})")
          true
        } else {
          false
        }
    }

    def reset(): Unit = {
      variables = mutable.Map.from(origVar)
      expressions = mutable.Map.from(origExp)
    }

    var variables: mutable.Map[String, Long] = mutable.Map.from(origVar)
    var expressions: mutable.Map[String, Expression] = mutable.Map.from(origExp)
    def solveUntil(node: String): Long = {
      while (!variables.contains(node)) {
        expressions.values.foreach(this.solve)
      }
      variables(node)
    }

    def solveAsMuchAsPossible(): Unit = {
      while (expressions.values.exists(solve)) {}
    }

    def getFullExpression(name: String): String = {
      variables.get(name).map(_.toString).getOrElse {
        if (name != "humn") {
          val exp = expressions(name)
          s"(${getFullExpression(exp.arg1Name)} ${exp.operator} ${getFullExpression(exp.arg2Name)})"
        } else {
          "x"
        }
      }
    }

    /**
     * I originally solved this by just printing out the equation and plugging it into wolfram
     * But it's a linear function so I figured I just solve it here with a bin search
     */
    def solvePart2(): Long = {
      val (p1, p2) = (expressions("root").arg1Name, expressions("root").arg2Name)

      def computeX(x: Long): Long = {
        reset()
        variables.remove("humn")
        variables("humn") = x
        solveUntil(p1) - solveUntil(p2)
      }

      var testMax = Long.MaxValue / 100000
      var test = 0L
      var testMin = Long.MinValue / 100000
      val origOutput = computeX(0)
      while (computeX(test) == origOutput) {
        test += Long.MaxValue / 10000000
      }
      val increasing = (computeX(test) - origOutput) > 0
      test = 0
      while(computeX(test) != 0) {
        val output: Long = computeX(test)
        val minRange = Math.abs(test - testMin) / 2
        val maxRange = Math.abs(test - testMax) / 2
        println(s"Testing $test => $output")
        if (output > 0L) {
          if (increasing) {
            // Decrease Test
            testMax = test
            test -= minRange
          } else {
            // Increase Test
            testMin = test
            test += maxRange
          }
        } else {
          if (increasing) {
            // Increase Test
            testMin = test
            test += maxRange
          } else {
            // Decrease Test
            testMax = test
            test -= minRange
          }
        }
      }
      test
    }

  }

}

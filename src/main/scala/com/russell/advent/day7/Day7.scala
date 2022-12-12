package com.russell.advent.day7

import scala.collection.mutable
import scala.io.Source

object Day7 {

  def parseDirs(path: String): (Seq[Seq[String]], mutable.Map[Seq[String], Int]) = {
    parseDirs(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseDirs(statements: Seq[String]): (Seq[Seq[String]], mutable.Map[Seq[String], Int]) = {
    var currentDir: Seq[String] = Seq()
    var dirs: Seq[Seq[String]] = Seq()
    val sizes: mutable.Map[Seq[String], Int] = mutable.Map().withDefaultValue(0)
    for (line <- statements) {
      line match {
        case command: String if command.startsWith("$") => command match {
          case cd if cd.contains("cd") =>
            val cdCommand = cd.split(" ")
            val target = cdCommand(2)
            if (target == "..") {
              currentDir = currentDir.take(currentDir.length - 1)
            } else {
              currentDir = currentDir :+ target
              dirs = dirs :+ currentDir
            }
          case _ =>
        }
        case lsEntry: String if !lsEntry.startsWith("dir") =>
          val entry = lsEntry.split(" ")
          val (size, name) = (entry(0).toInt, entry(1))
          currentDir = currentDir :+ name
          for (dirLen <- 1 to currentDir.size) {
            sizes(currentDir.take(dirLen)) += size
          }
          currentDir = currentDir.take(currentDir.length - 1)
        case _ =>
      }
    }
    (dirs, sizes)
  }

}

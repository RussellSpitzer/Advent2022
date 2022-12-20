package com.russell.advent.day19

import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.collection.parallel.CollectionConverters._
object Day19 {

  val BIG = 10000
  def parseInput(path: String): Seq[Plan] = {
    parseInput(Source.fromResource(path).getLines().map(_.trim).toSeq)
  }

  def parseInput(lines: Seq[String]): Seq[Plan] = {
    lines.map{ line =>
      Plan(
        line
          .replaceAll("Blueprint .*: ", "")
          .replaceAll("\\s*Each \\w+ robot costs ", "")
          .replaceAll("\\s*and\\s*", ",")
          .split('.')
          .map( str => new ElementCollection(str))
      )
    }
  }

  object Element extends Enumeration {
    type Element = Value
    val Ore, Clay, Obsidian, Geode = Value
  }
  import Element._

  val Elements = Seq(Ore, Clay, Obsidian, Geode).toArray
  val ElementIds = Elements.map(_.id)

  case class ElementCollection(elements: Array[Int]) {
    def this(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0) = {
      this(Array.fill(4)(0))
      this.elements(Ore.id) = ore
      this.elements(Clay.id) = clay
      this.elements(Obsidian.id) = obsidian
      this.elements(Geode.id) = geode
    }

    def this(str: String) = {
      this(Array.fill(4)(0))
      str.split(',').foreach{ e =>
        val arr = e.split(' ')
        val num = arr(0).toInt
        val element = Element.withName(arr(1)(0).toUpper+arr(1).slice(1, arr(1).length))
        elements(element.id) +=  num
      }
    }

    def -(that: ElementCollection): ElementCollection = {
      ElementCollection(ElementIds.map(id => this.elements(id) - that.elements(id)))
    }

    def +(that: ElementCollection): ElementCollection = {
      ElementCollection(ElementIds.map(id => this.elements(id) + that.elements(id)))
    }

    def +(that: Element): ElementCollection = {
      val clonedArray = this.elements.clone()
      clonedArray(that.id) += 1
      this.copy(clonedArray)
    }

    def *(that: Int): ElementCollection = {
      ElementCollection(ElementIds.map(id => this.elements(id) * that))
    }


    def +=(that: ElementCollection): Unit = {
      ElementIds.foreach(id => this.elements(id) += that.elements(id))
    }

    def +=(element: Element): Unit = {
      this.elements(element.id) += 1
    }

    def -=(that: ElementCollection): Unit = {
      ElementIds.foreach(id => this.elements(id) -= that.elements(id))
    }

    def <(that: ElementCollection): Boolean = {
      ElementIds.forall(id => this.elements(id) < that.elements(id))
    }

    def <=(that: ElementCollection): Boolean = {
      ElementIds.forall(id => this.elements(id) <= that.elements(id))
    }

    def >(that: ElementCollection): Boolean = {
      ElementIds.forall(id => this.elements(id) > that.elements(id))
    }

    def >=(that: ElementCollection): Boolean = {
      ElementIds.forall(id => this.elements(id) >= that.elements(id))
    }

    def /(that: ElementCollection): Int = {
      if (this.elements.forall( _ <= 0)) {
        0
      } else {
        Try {
          val divisors = Elements.map(element =>
            if (this (element) > 0 && that(element) > 0) {
              Math.ceil(this (element).doubleValue / that(element).doubleValue).toInt
            } else if (this (element) > 0 && that(element) == 0) {
              BIG
            } else {
              0
            }
          )
          divisors.max
        }.getOrElse(BIG)
      }
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case that: ElementCollection => ElementIds.forall(id => this.elements(id) == that.elements(id))
        case _ => super.equals(obj)
      }
    }

    def firstMissingElement(): Element = {
      if (this.elements(Ore.id) < 0) {
        Ore
      } else if (this.elements(Clay.id) < 0) {
        Clay
      } else if (this.elements(Obsidian.id) < 0) {
        Obsidian
      } else {
        Geode
      }
    }

    def apply(element: Element): Int = {
      elements(element.id)
    }
  }
  case object ElementCollection {
    def apply(): ElementCollection = {
      ElementCollection(Array.fill(4)(0))
    }

    def apply(ore: Int, clay: Int, obsidian: Int, geode: Int): ElementCollection = {
      new ElementCollection(ore, clay, obsidian, geode)
    }
  }

  case class Plan(costs: Array[ElementCollection])
  case class World(var plan: Plan,
                   inventory: ElementCollection = ElementCollection(),
                   robots: ElementCollection = ElementCollection(1,0,0,0),
                   var timeLeft: Int = 24) {
    def tick(choices: mutable.Queue[Element]): World = {
      println(s"Starting tick ${25 - timeLeft} - trying to build ${choices.headOption} - Inventory ${inventory.elements.mkString(",")}")
      val choice = choices.headOption
      val canBuild = choice.map(consumeRobotResources).getOrElse(false)
      inventory += robots
      if (canBuild) {
        buildRobot(choices.dequeue())
      }
      println(s"Tick Over - Robots: ${robots.elements.mkString(",")} - Inventory ${inventory.elements.mkString(",")}")
      timeLeft -= 1
      this
    }

    def run(choices: Seq[Element]): World = {
      val choiceQueue = mutable.Queue().addAll(choices)
      while (timeLeft > 0) {
        this.tick(choiceQueue)
      }
      this
    }

    def consumeRobotResources(element: Element): Boolean = {
      val canBuild = plan.costs(element.id) <= inventory
      if (canBuild) {
        inventory -= plan.costs(element.id)
      }
      canBuild
    }

    def buildRobot(element: Element): Unit = {
      println(s"Building a $element Robot")
      robots += element
    }
  }

  case class State(inventory: ElementCollection = ElementCollection(),
                   robots: ElementCollection = ElementCollection(1,0,0,0),
                   timeLeft: Int = 24) {
    override def toString: String = {
      s"${inventory(Geode)} : $timeLeft "
    }
  }

  def bestToBuild(plan: Plan, timeLeft: Int = 24): Int = {
    val initialState = State(timeLeft = timeLeft)
    val toVisit: mutable.Queue[State] = mutable.Queue(initialState)
    var mostGems = 0
    var mostGemsState: State = initialState

    while (toVisit.nonEmpty) {
      val state = toVisit.dequeue()
      val gemsIfWeDidNothing = state.robots(Geode) * state.timeLeft
      val finalGems = state.inventory(Geode) + gemsIfWeDidNothing
      if (finalGems  > mostGems) {
        mostGems = finalGems
        val finalInventory = state.inventory + ElementCollection(0,0,0, gemsIfWeDidNothing)
        mostGemsState = state.copy(timeLeft = 0, inventory = finalInventory)
        println(s"New Best State : ${mostGemsState}")
      }
      val nextTests: Seq[Element] = if (state.robots > plan.costs(Geode.id)) {
        Seq(Geode)
      } else {
        Elements.reverse
      }
      nextTests.foreach { nextElement =>
        val requiredMaterials = plan.costs(nextElement.id) - state.inventory
        val timeToBuild = requiredMaterials / state.robots + 1
        val timeAfter = Math.max(state.timeLeft - timeToBuild, 0)
        val maxFutureGeodes = (timeAfter * ((timeAfter) / 2 + 1)) + (state.timeLeft * state.robots(Geode)) + state.inventory(Geode)
        if (maxFutureGeodes > mostGems && timeToBuild < state.timeLeft) {
          val afterBuild = state.timeLeft - timeToBuild
          val newInventory = (state.inventory - plan.costs(nextElement.id)) + (state.robots * timeToBuild)
          val newState = State( newInventory, state.robots + nextElement, afterBuild)
          if (nextElement == Geode || nextElement == Obsidian) {
            toVisit.prepend(newState)
          } else {
            toVisit.enqueue(newState)
          }
        }
      }
    }
    mostGems
  }




}

package day06

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.collection.mutable

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day06")

def part1(input: String): Int =
  findIndex(input, n = 4)

def part2(input: String): Int =
  findIndex(input, n = 14)

def findIndex(input: String, n: Int): Int =
  val firstIndex = input.sliding(n).indexWhere(_.toSet.size == n)
  firstIndex + n

def findIndexOptimal(input: String, n: Int): Int =
  val counts = mutable.HashMap.empty[Char, Int]
  var previous = '#'
  val firstIndex = input.iterator.sliding(n).indexWhere(cs =>
    if previous == '#' then
      cs.foreach(counts(_) = 1)
    else if previous != cs.last then
      counts.updateWith(previous) {
        case Some(value) => if value == 1 then None else Some(value - 1)
        case None => None // don't care if not in the
      }
      counts.updateWith(cs.last) {
        case Some(value) => Some(value + 1)
        case None => Some(1)
      }
    previous = cs.head
    counts.size == n
  )
  firstIndex + n

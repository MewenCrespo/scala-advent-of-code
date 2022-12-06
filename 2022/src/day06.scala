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
  findIndexOptimal(input, n = 4)

def part2(input: String): Int =
  findIndexOptimal(input, n = 14)

def findIndex(input: String, n: Int): Int =
  val firstIndex = input.sliding(n).indexWhere(_.toSet.size == n)
  firstIndex + n

class MultiSet:
  private val counts = new Array[Int](128)
  private var total = 0
  def size = total

  def add(c: Char) =
    val count = counts(c.toInt)
    if count == 0 then
      total += 1
    counts(c.toInt) += 1

  def remove(c: Char) =
    val count = counts(c.toInt)
    if count > 0 then
      if count == 1 then
        total -= 1
      counts(c.toInt) -= 1
end MultiSet

def findIndexOptimal(input: String, n: Int): Int =
  val counts = MultiSet()
  var i = -1
  var previous = '#'
  val windows = input.iterator.sliding(n)
  while windows.hasNext && {
    val window = windows.next()
    if previous == '#' then
      window.foreach(counts.add)
    else if previous != window.last then
      counts.remove(previous)
      counts.add(window.last)
    previous = window.head
    i += 1
    counts.size != n
  } do ()
  i + n

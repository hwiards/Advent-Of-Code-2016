package day03

import scala.io.Source

object Day03 {

  def main(args: Array[String]): Unit = {

    val input = Source.fromResource("day03.txt").getLines().map(_.trim.split("\\s+").map(_.toInt)).toArray

    val part1 = input.count(check(_))
    println(f"Part 1: $part1")

    val part2 = _part2(input)
    println(f"Part 2: ${part2}")
  }

  def _part2(input: Array[Array[Int]]): Int = {
    var erg2 = 0

    for(rowBlock <- 0 until(input.length, 3)){
      for(column <- 0 until 3){
        val arr = Array(input(rowBlock)(column), input(rowBlock+1)(column), input(rowBlock+2)(column))
        if(check(arr)) erg2 += 1
      }
    }
    return erg2
  }

  def check(sides: Array[Int]):Boolean ={

    val sorted = sides.sorted
    if(sorted(0) + sorted(1) > sorted(2)) true
    else false
  }
}
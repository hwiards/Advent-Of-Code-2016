package day01

import scala.collection.mutable.HashSet
import scala.io.Source

object Day01 {

  //Hashcode and Equals are only dependant on the first parameter section.
  case class Position(x: Int, y: Int)(dir: Direction){

    def move(len: Int, right: Boolean, straight: Boolean): Position = {
      val newDir = if(straight) dir else (if(right) dir.turnRight() else dir.turnLeft())
      Position(x + newDir.x*len, y + newDir.y*len)(newDir)
    }

    def move(moveStr: String):Position = {
      val right = moveStr(0) == 'R'
      val len = moveStr.substring(1).toInt
      move(len, right, false)
    }

    def dist = x.abs + y.abs

  }

  case class Direction(x: Int, y: Int) {

    def turnRight(): Direction = {
      Direction(y, x * -1)
    }

    def turnLeft(): Direction = {
      Direction(y * -1, x)
    }
  }

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("day01.txt").getLines().toArray
    val moveStrings = lines(0).split(", ")

    val startPos = Position(0,0)(Direction(0,1))
    val erg1 = moveStrings.foldLeft(startPos)((pos, moveStr) => pos.move(moveStr)).dist

    println(f"Part 1: $erg1")
    println(f"Part 2: ${part2(moveStrings)}")
  }

  def part2(moveStrings: Array[String]): Int ={

    val visitedSet = HashSet[Position]()
    var visited = false

    var aktPos = Position(0,0)(Direction(0,1))
    visitedSet.add(aktPos)

    for(moveStr <- moveStrings){
      val right = moveStr(0) == 'R'
      val len = moveStr.substring(1).toInt

      for(i <- 0 until len){
        if(i == 0) aktPos = aktPos.move(1,right,false)
        else aktPos = aktPos.move(1,false,true)

        if(visitedSet.contains(aktPos)) return aktPos.dist
        visitedSet.add(aktPos)
      }
    }
    return -1
  }
}

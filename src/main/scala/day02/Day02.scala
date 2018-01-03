package day02

import Utils.Direction

import scala.io.Source

object Day02 {

  val up = Direction(-1,0)
  val down = Direction(1,0)
  val right = Direction(0,1)
  val left = Direction(0,-1)

  case class Position(x: Int, y: Int)(grid:Array[Array[Char]]){

    def move(dir: Direction): Position = {
      //Check if running onto dot
      if(grid(x+dir.x)(y+dir.y) != '.') Position(x+dir.x,y+dir.y)(grid)
      else this
    }
  }

  def main(args: Array[String]): Unit = {

    val input = Source.fromResource("day02/day02.txt").getLines().toArray

    //Surround the Input with dots to check the boundaries.
    val grid1 = Source.fromResource("day02/day02a.txt").getLines().map(_.toCharArray).toArray
    val grid2 = Source.fromResource("day02/day02b.txt").getLines().map(_.toCharArray).toArray

    println(f"Part 1: ${solve(grid1, input)}")
    println(f"Part 1: ${solve(grid2, input)}")
  }

  def solve(grid: Array[Array[Char]], input: Array[String]): String = {
    var aktPos = findFive(grid)

    var solution = ""

    for(line <- input){
      for(dir <- line){
        dir match {
          case 'U' => aktPos = aktPos.move(up)
          case 'R' => aktPos = aktPos.move(right)
          case 'L' => aktPos = aktPos.move(left)
          case 'D' => aktPos = aktPos.move(down)
        }
      }
      solution += grid(aktPos.x)(aktPos.y)
    }
    solution
  }

  //You start always at 5. My Input allowed in Part 2 for starting in the middle..
  def findFive(grid: Array[Array[Char]]): Position ={
    for(i <- grid.indices){
      for(j <- grid(i).indices){
        if(grid(i)(j) == '5') return Position(i,j)(grid)
      }
    }
    Position(grid.length/2, grid.length/2)(grid)
  }

}
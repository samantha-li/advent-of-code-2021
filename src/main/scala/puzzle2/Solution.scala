package puzzle2

import scala.io.Source

object Solution {
  val input: Seq[(Int, Int)] = Source.fromResource("input2.txt")
    .getLines
    .toSeq
    .map { s => 
      val arr = s.split(" ")
      arr(0) match {
        case "forward" => (arr(1).toInt, 0)
        case "up" => (0, -arr(1).toInt)
        case "down" => (0, arr(1).toInt)
      }
    }
  
  def partOne: Int = {
    val (xpos, hpos) = input.foldLeft((0, 0)) {
      case ((xpos, hpos), (x, h)) => (xpos + x, hpos + h)
    }
    xpos * hpos
  }

  def partTwo: Int = {
    val (xpos, hpos, _) = input.foldLeft((0, 0, 0)) {
      case ((xpos, hpos, aim), (x, h)) =>
        (xpos + x, hpos + (aim * x), aim + h)
    }
    xpos * hpos
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}
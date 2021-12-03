package puzzle1

import scala.io.Source

object Solution {
  val input: Seq[Int] = Source.fromResource("input1.txt")
    .getLines
    .toSeq
    .map(_.toInt)

  def partOne: Int = {
    input.tail.foldLeft((input.head, 0)){
      case ((last, count), curr) => if (curr > last) (curr, count + 1) else (curr, count)
    }._2
  }

  def partTwo: Int = {
   input.dropRight(3).zip(input.drop(3)).count {
     case (first, next) => next > first
   }
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}
package puzzle5

import scala.io.Source

object Solution {
  val input = Source.fromResource("input5.txt")
    .getLines
    .toSeq
    .map { line =>
      val arr = line.split(" -> ").map { pair =>
        val arr = pair.split(",")
        (arr(0).toInt, arr(1).toInt)
      }
      (arr(0), arr(1))
    }

  val xMax = (input.map(_._1._1) ++ input.map(_._2._1)).max
  val yMax = (input.map(_._1._2) ++ input.map(_._2._2)).max

  def partOne: Int = {
    input.filter {
      case ((x1, y1), (x2, y2)) => x1 == x2 || y1 == y2
    }.foldLeft(Map.empty[(Int, Int), Int]) {
      case (map, ((x1, y1), (x2, y2))) if x1 == x2 =>
        { if (y1 < y2) y1 to y2 else y2 to y1 }.foldLeft(map) {
          case (m, y) =>
            m ++ Map((x1, y) -> (m.getOrElse((x1, y), 0) + 1))
        }
      case (map, ((x1, y1), (x2, y2))) if y1 == y2 =>
        { if (x1 < x2) x1 to x2 else x2 to x1 }.foldLeft(map) {
          case (m, x) =>
            m ++ Map((x, y1) -> (m.getOrElse((x, y1), 0) + 1))
        }
    }.count(_._2 >= 2)
  }

  def partTwo: Int = {
    val map = input.foldLeft(Map.empty[(Int, Int), Int]) {
      case (map, ((x1, y1), (x2, y2))) if x1 == x2 =>
        { if (y1 < y2) y1 to y2 else y2 to y1 }.foldLeft(map) {
          case (m, y) =>
            m ++ Map((x1, y) -> (m.getOrElse((x1, y), 0) + 1))
        }
      case (map, ((x1, y1), (x2, y2))) if y1 == y2 =>
        { if (x1 < x2) x1 to x2 else x2 to x1 }.foldLeft(map) {
          case (m, x) =>
            m ++ Map((x, y1) -> (m.getOrElse((x, y1), 0) + 1))
        }
      case (map, ((x1, y1), (x2, y2))) =>
        val xRange = { if (x1 < x2) x1 to x2 else (x2 to x1).reverse }
        val yRange = { if (y1 < y2) y1 to y2 else (y2 to y1).reverse }
        xRange.zip(yRange).foldLeft(map) {
          case (m, (x, y)) =>
           m ++ Map((x, y) -> (m.getOrElse((x, y), 0) + 1))
        }
    }
    map.count(_._2 >= 2)
  }

  def printer(map: Map[(Int, Int), Int]): Unit = {
    val filled = (0 to xMax).foldLeft(map) {
      case (m, x) => (0 until yMax).foldLeft(m) {
        case (mm, y) => mm ++ Map((x, y) -> (mm.getOrElse((x, y), 0)))
      }
    }
    print(filled)
    val xGrouped: Map[Int, Seq[((Int, Int), Int)]] = filled.toSeq.groupBy(_._1._2)
    xGrouped.toSeq.map {
      case (k, v) => k -> v.sortBy(_._1)
    }.sortBy(_._1).foreach {
      case (_, row) =>
        println(row)
        row.foreach{ case (_, num) => if (num == 0) print(".") else print(num) }
        println("")
    }
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}
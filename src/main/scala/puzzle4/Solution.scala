package puzzle4

import scala.io.Source

object Solution {
  case class Cell(boardId: Int, x: Int, y: Int, num: Int)

  val input = Source.fromResource("input4.txt")
    .getLines
    .toSeq

  val calls = input.head.split(",").map(_.toInt)

  val boards: Seq[Cell] = input.tail.sliding(6, 6).zipWithIndex.foldLeft(Seq.empty[Cell]) {
    case (cells, (lines, boardId)) =>
    lines.tail.zipWithIndex.foldLeft(cells) {
      case (cells, (row, y)) => row.split(" ").filter(_.nonEmpty).zipWithIndex.foldLeft(cells) {
        case (cells, (num, x)) => cells ++ Seq(Cell(boardId, x, y, num.toInt))
      }
    }
  }

  def partOne: Int = {
    val (sum, winningNumber, _) = calls.foldLeft((-1, -1, Seq.empty[Cell])) {
      case ((done, winning, called), _) if done != -1 => (done, winning, called)
      case ((_, _, called), call) =>
        val newCalled = called ++ boards.filter(_.num == call)
        val xBingo = newCalled.groupBy(c => (c.boardId, c.x)).values.filter(_.size == 5).flatten
        val yBingo = newCalled.groupBy(c => (c.boardId, c.y)).values.filter(_.size == 5).flatten
        val sum = {
          if (xBingo.nonEmpty) {
            boards.filter(c => c.boardId == xBingo.head.boardId && !newCalled.map(_.num).contains(c.num)).map(_.num).sum
          } else if (yBingo.nonEmpty) {
            boards.filter(c => c.boardId == yBingo.head.boardId && !newCalled.map(_.num).contains(c.num)).map(_.num).sum
          } else -1
        }
        (sum, call, newCalled)
    }
    sum * winningNumber
  }

  def partTwo: Int = {
    val filtered = boards.filter(c => calls.contains(c.num))
    val allWinners = filtered.groupBy(c => (c.boardId, c.x)).filter(_._2.length == 5).map(_._1._1).toSet ++
      filtered.groupBy(c => (c.boardId, c.y)).filter(_._2.length == 5).map(_._1._1).toSet
    val (sum, winningNumber, _) = calls.reverse.foldLeft((-1, -1, filtered.filter(c => allWinners.contains(c.boardId)))) {
      case ((done, winning, called), _) if done != -1 => (done, winning, called)
      case ((_, _, called), call) =>
        val newCalled = called.filterNot(_.num == call)
        val xBingo = newCalled.groupBy(c => (c.boardId, c.x)).filter(_._2.size == 5).keys.map(_._1)
        val yBingo = newCalled.groupBy(c => (c.boardId, c.y)).filter(_._2.size == 5).keys.map(_._1)
        val winners = (xBingo ++ yBingo).toSet
        val losers = boards.filter(c => !winners.contains(c.boardId))
        println(losers)
        val sum = if (losers.nonEmpty) boards.filter(c => c.boardId == losers.head.boardId && !newCalled.map(_.num).contains(c.num)).map(_.num).sum - call
        else -1
        (sum, call, newCalled)
    }
    sum * winningNumber
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}
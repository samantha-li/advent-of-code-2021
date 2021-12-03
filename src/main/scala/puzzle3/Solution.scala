package puzzle3

import scala.io.Source

object Solution {
  val input = Source.fromResource("input3.txt")
    .getLines
    .toSeq
    .map(_.toCharArray().toSeq)

  val len = input.head.length
  val fileLen = input.length

  def toDecimal(seq: Seq[Char]): BigInt = {
    seq.reverse.zipWithIndex.foldLeft(BigInt(0)) {
      case (acc, (c, idx)) => c match {
        case '1' => acc + BigInt(2).pow(idx)
        case _ => acc
      }
    }
  }

  def partOne: BigInt = {
    val counts =
      input.map(_.reverse).foldLeft(Map.empty[Int, Int]) {
        case (m, bin) =>
          bin.zipWithIndex.foldLeft(m) {
            case (m1, (b, idx)) if b == '1' => m1 ++ Map[Int, Int](idx -> (m1.getOrElse(idx, 0) + 1))
            case (m1, _) => m1
          }
      }
    val (g, e) = counts.foldLeft((BigInt(0), BigInt(0))) {
      case ((g, e), (idx, count)) => if (count > fileLen / 2)
        (g + BigInt(2).pow(idx), e) else (g, e + BigInt(2).pow(idx))
    }
    g * e
  }

  def partTwo: BigInt = {
    def helperFunc(r: Seq[Seq[Char]], idx: Int, a: Char, b: Char): Char = {
      if (r.foldLeft(0){ case (acc, cs) => if (cs.drop(idx).head == '1') acc + 1 else acc } >= r.length / 2.0) a else b
    }

    val (o, c) = (0 until len).foldLeft((input, input)) {
      case ((r1, r2), _) if r1.length == 1 && r2.length == 1 =>
        (r1, r2)
      case ((r1, r2), idx) if r2.length == 1 =>
        val c1 = helperFunc(r1, idx, '1', '0')
        (r1.filter(_.drop(idx).head == c1), r2)
      case ((r1, r2), idx) if r1.length == 1 =>
        val c2 = helperFunc(r2, idx, '0', '1')
        (r1, r2.filter(_.drop(idx).head == c2))
      case ((r1, r2), idx) =>
        val c1 = helperFunc(r1, idx, '1', '0')
        val c2 = helperFunc(r2, idx, '0', '1')
        (r1.filter(_.drop(idx).head == c1), r2.filter(_.drop(idx).head == c2))
    }

    toDecimal(o.head) * toDecimal(c.head)
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}
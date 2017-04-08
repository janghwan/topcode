import lib.CodeJam

import scala.annotation.tailrec
import scala.collection.mutable

class R1AA2016(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val N = br.readLine.toInt

    val in : Array[Set[List[Int]]] = Array.ofDim[Set[List[Int]]](2500)

    for (i <- 0 until N) {
      val line = br.readLine().split("\\s+").map(_.toInt)
      in(line(0)) = in(line(0)) + line.toList
    }

    val mat = Array.ofDim[Int](N,N)

    for (i <- 0 until N) {}
//    printResult(out.mkString(""))
  }

  def init(): Unit = {

  }
}
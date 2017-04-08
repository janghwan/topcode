package hackercup.y2017.qualification

import lib.CodeJam

/**
  * Created by essis on 2016. 1. 8..
  */
class B(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val N = br.readLine.toInt
    val arr = Array.ofDim[Int](N)
    for (i <- 0 until N) {
      arr(i) = br.readLine().toInt
    }

    var tail = N
    val minW = 50.0
    var denom = 1
    var move = 0
    val bags = arr.sortWith(_ > _)
    for ((w, i) <- bags.view.zipWithIndex) {
      if (w / Math.ceil(minW / denom) < 1) {
        while(w / Math.ceil(minW / denom) < 1) {
          denom += 1
        }
      }
      tail -= (denom - 1)
      if (tail <= i) {
        printResult(move.toString)
        return
      }
      move += 1
    }

    if (move == 0) move += 1

    printResult(move.toString)
  }


  def init(): Unit = {

  }
}
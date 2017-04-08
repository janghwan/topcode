package hackercup.y2017.R1

import lib.CodeJam

import scala.annotation.tailrec

/**
  * Created by essis on 2016. 1. 8..
  */
class A(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)
    val (n, m) = (row(0), row(1))

    val priceM = Array.ofDim[Int](n, m)
    for (i <- 0 until n) {
      priceM(i) = br.readLine.split("\\s+").map(_.toInt).sortWith(_<_).array
    }

    val purchase = Array.ofDim[Int](n)
    var total = 0
    for (i <- 0 until n) {
      val (mi, min) = findMin(0, i+1, Int.MaxValue, 0)
      purchase(mi) += 1
      total += min
    }

    printResult(total.toString)

    @tailrec
    def findMin(i: Int, ilimit: Int, min: Int, pi: Int): (Int, Int) = {
      if (i == ilimit) (pi, min)
      else {
        val count = purchase(i)
        if (count < m) {
          val temp = priceM(i)(count) - count * count + (count+1)*(count+1)
          if (temp < min) findMin(i+1, ilimit, temp, i)
          else findMin(i+1, ilimit, min, pi)
        } else {
          findMin(i+1, ilimit, min, pi)
        }
      }
    }
  }


  def init(): Unit = {

  }
}
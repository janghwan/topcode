package hackercup.y2020.qualification

import lib.CodeJam

import scala.collection.immutable.TreeMap

class Running1(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val arr = br.readLine.split(" ").map(_.toInt)
    val N = arr(0)
    val M = arr(1)
    val C = Array.ofDim[Int](N+1)

    for (i <- 1 to N)
        C(i) = br.readLine.toInt

    val m = Array.ofDim[Int](N+1)
    val c = Array.ofDim[Long](N+1)

    m(1) = M
    c(1) = 0
    for (i <- 2 to N) {
      if (m(i-1) > 0) {
        m(i) = m(i-1) - 1
        c(i) = c(i-1)
      } else {
        var minc = Long.MaxValue
        var minj = 0
        for (j <- i - M until i) {
          if (C(j) == 0 || c(j) == -1) ()
          else {
            val tempc = C(j) + c(j)
            if (tempc <= minc) {
              minj = j
              minc = tempc
            }
          }
        }
        if (minc == Long.MaxValue) {
          c(i) = -1
          m(i) = -1
        } else {
          c(i) = minc
          m(i) = M - (i - minj)
        }
      }
    }


    printResult(c(N).toString)


  }


  def init(): Unit = {

  }
}
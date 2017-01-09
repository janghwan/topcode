package hackercup.y2016.qualification

import lib.CodeJam

import scala.annotation.tailrec

/**
  * Created by essis on 2016. 1. 8..
  */
class A(fileName: String) extends CodeJam(fileName) {


  @tailrec
  private def factorial(n: Long, accumulator: BigInt = 1): BigInt = {
    if(n == 0) accumulator else factorial(n - 1, accumulator * n)
  }

  def solve(): Unit = {
    val row: Array[Long] = br.readLine.split("\\s+").map(_.toLong)

    val N = row(0).toInt

    if (N < 3) {
      printResult("0")
      return
    }

    // init array
    val stars = Array.ofDim[(Long,Long)](N)
    for (i <- 0 until N) {
      val line = br.readLine.split("\\s+").map(_.toLong)
      stars(i) = (line(0), line(1))
    }
    // calculate distance

    val dist = Array.ofDim[Long](N,N)

    for (i <- 0 until N) {
      for (j <- 0 until N) {
        val dx = stars(i)._1 - stars(j)._1
        val dy = stars(i)._2 - stars(j)._2
        dist(i)(j) =  dx * dx + dy * dy
      }
    }

    // sort rows

    var sum = BigInt(0)

    for (i <- 0 until N) {
      val sorted = dist(i).sortWith(_<_)
      sum = rec(sorted, sum, 0)
    }

    printResult(sum.toString)

    // count boomerangs.
    @tailrec
    def rec(arr: Array[Long], sum: BigInt, start: Int): BigInt = {
      if (arr.length < 2 || start >= N)
        return sum
      var i = start + 1
      while (i < N && arr(start) == arr(i) ) {
        i += 1
      }

      val span = i - start
      val count = if (span > 1)
        factorial(span) / 2
      else
        BigInt(0)
      rec(arr, count + sum, i)
    }
  }


  def init(): Unit = {

  }
}
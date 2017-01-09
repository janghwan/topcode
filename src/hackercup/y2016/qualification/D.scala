package hackercup.y2016.qualification

import lib.CodeJam

/**
  * Created by essis on 2016. 1. 8..
  */
class D(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)
    val N = row(0)
    val K = row(1)

    val arr: Array[String] = Array.ofDim[String](N)

    for (i <- 0 until N) {
      arr(i) = br.readLine()
    }

    val sorted = arr.sortWith(_<_)


    println(sorted.mkString("\n"))
//    printResult(count.toString)

  }

  def distance(a: String, b: String): Int = {
    val lena = a.length
    val lenb = b.length
    val length = Math.max(lena, lenb)
    var i = 0
    while(i < lena && i < lenb && a(i) == b(i)) {
      i += 1
    }

    lena + lenb - 2 * i
  }

  def init(): Unit = {

  }
}
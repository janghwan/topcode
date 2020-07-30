package hackercup.y2020.qualification

import lib.CodeJam

import scala.collection.immutable.TreeMap

class Timber(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val N: Int = br.readLine.toInt
    var map = TreeMap[Int, Int]()

    for (i <- 0 until N) {
      val C = br.readLine.split(' ').map(_.toInt)
      map = map + (C(0) -> C(1))
    }
    var dp = TreeMap[Int, Int]()

    for ((p, h) <- map) {
     val zero = dp.getOrElse(p, 0)
     val l = dp.getOrElse(p - h, 0)
     // left
     dp += (p -> Math.max(zero, l + h))
     // right
     val r = dp.getOrElse(p + h, 0)
     dp += (p + h -> Math.max(r, zero + h))
    }

    printResult(dp.values.max.toString)
  }


  def init(): Unit = {

  }
}
package hackercup.y2020.round2

import lib.CodeJam

class Capastaty(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val N = sc.nextInt()
    val K = sc.nextInt()
    sc.nextLine()
    val S = sc.nextLine.split(" ").map(_.toLong)
    val AS = sc.nextLong()
    val BS = sc.nextLong()
    val CS = sc.nextLong()
    val DS = sc.nextLong()
    sc.nextLine()
    val X = sc.nextLine.split(" ").map(_.toLong)
    val AX = sc.nextLong()
    val BX = sc.nextLong()
    val CX = sc.nextLong()
    val DX = sc.nextLong()
    sc.nextLine()
    val Y = sc.nextLine.split(" ").map(_.toLong)
    val AY = sc.nextLong()
    val BY = sc.nextLong()
    val CY = sc.nextLong()
    val DY = sc.nextLong()
    sc.nextLine()

    var long = 0L
    var short = 0L
    var mid = 0L
    var buff = 0L
    var recentS = 0L
    var rrecentS = 0L
    var recentX = 0L
    var rrecentX = 0L
    var recentY = 0L
    var rrecentY = 0L

    for (i <- 0 until N) {
      val s = if (i < K) S(i)
      else (AS * rrecentS + BS * recentS + CS) % DS
      val x = if (i < K) X(i)
      else (AX * rrecentX + BX * recentX + CX) % DX
      val y = if (i < K) Y(i)
      else (AY * rrecentY + BY * recentY + CY) % DY
      short += Math.max(x - s, 0)
      long += Math.max(s - x - y, 0)
      mid += Math.min(Math.max(s - x, 0), y)
      buff += y
      rrecentS = recentS
      recentS = s
      rrecentX = recentX
      recentX = x
      rrecentY = recentY
      recentY = y
    }

    if (short > long + mid || long > short + buff - mid) {
      printResult("-1")
    } else {
      printResult(Math.max(short, long).toString)
    }

  }


  def init(): Unit = {

  }
}
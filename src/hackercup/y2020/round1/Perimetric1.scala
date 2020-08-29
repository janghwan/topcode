package hackercup.y2020.round1

import lib.CodeJam

class Perimetric1(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val N = sc.nextInt()
    val K = sc.nextInt()
    val W = sc.nextInt()
    sc.nextLine()
    val L = sc.nextLine.split(" ").map(_.toLong)
    val AL = sc.nextLong()
    val BL = sc.nextLong()
    val CL = sc.nextLong()
    val DL = sc.nextLong()
    sc.nextLine()
    val H = sc.nextLine.split(" ").map(_.toLong)
    val AH = sc.nextLong()
    val BH = sc.nextLong()
    val CH = sc.nextLong()
    val DH = sc.nextLong()
    sc.nextLine()
    var P = 0L
    var ans = 0L // result
    var recentL = 0L
    var rrecentL = 0L
    var recentH = 0L
    var rrecentH = 0L
    var recentHeight = Array.ofDim[Long](W)
    val m = 1000000007
    for (i <- 0 until N) {
      val l = if (i < K) L(i)
        else (AL * rrecentL + BL * recentL + CL) % DL + 1
      val h = if (i < K) H(i)
        else (AH * rrecentH + BH * recentH + CH) % DH + 1

      val inc = updateRecent(l, h)

      rrecentL = recentL
      recentL = l
      rrecentH = recentH
      recentH = h
      ans = if (P == 0) inc else modProd(ans, P + inc)
      if (P + inc <0) {
        ()
      }
      P = P + inc

//      print(P + " ")
    }
//    println()
    printResult(ans.toString)

    def modProd(a: Long, b: BigInt): Int = {
      val ret = ((a % m) * (b % m)) % m
      ret.toInt
    }

    def updateRecent(L: Long, H: Long): Long = {
      var newRecentHeight: Array[Long] = null
      if (L < recentL + W) {
        val start = (L - recentL).toInt
        newRecentHeight = recentHeight.slice(start, W) ++ Array.fill(start)(H)
        for (i <- 0 until W - start) {
          if (newRecentHeight(i) < H) newRecentHeight(i) = H
        }
        val p1 = peri(recentHeight, start)
        val p2 = peri(newRecentHeight, 0)
        val left1 = recentHeight(start-1)
        val left2 = recentHeight(start)
        val leftWall = if (left1 > left2) {
          if (H > left1) H - left1
          else if (H > left2) left1 - H
          else left1 - left2
        } else if (H > left2) H - left1
        else left2 - left1
        val rightWall = newRecentHeight.last - recentHeight.last
        recentHeight = newRecentHeight
        p2 - p1 + leftWall + rightWall
      } else {
        val leftWall = if (recentL + W == L) Math.min(recentHeight(W-1), H) else 0
        recentHeight = Array.fill(W)(H)
        peri(recentHeight, 0) + recentHeight.head + recentHeight.last - 2 * leftWall
      }
    }


  }

  def peri(arr: Array[Long], from: Int): Long = {
    var ret = 0L
    for (i <- from until arr.length) {
      if (arr(i) > 0) ret += 2
      if (i > 0)
        ret += Math.abs(arr(i - 1) - arr(i))
    }
    ret
  }


  def init(): Unit = {

  }
}
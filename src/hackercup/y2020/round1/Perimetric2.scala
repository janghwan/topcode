package hackercup.y2020.round1

import lib.CodeJam

import scala.collection.mutable

class Perimetric2(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val N = sc.nextInt()
    val K = sc.nextInt()
    sc.nextLine()
    val L = sc.nextLine.split(" ").map(_.toLong)
    val AL = sc.nextLong()
    val BL = sc.nextLong()
    val CL = sc.nextLong()
    val DL = sc.nextLong()
    sc.nextLine()
    val W = sc.nextLine.split(" ").map(_.toLong)
    val AW = sc.nextLong()
    val BW = sc.nextLong()
    val CW = sc.nextLong()
    val DW = sc.nextLong()
    sc.nextLine()
    val H = sc.nextLine.split(" ").map(_.toLong)
    val AH = sc.nextLong()
    val BH = sc.nextLong()
    val CH = sc.nextLong()
    val DH = sc.nextLong()
    sc.nextLine()
    val ls = mutable.TreeSet[Long]()
    val rs = mutable.TreeSet[Long]()
    var P = 0L
    var ans = 0L // result
    var recentL = 0L
    var rrecentL = 0L
    var recentH = 0L
    var rrecentH = 0L
    var recentW = 0L
    var rrecentW = 0L
    val m = 1000000007
    for (i <- 0 until N) {
      val l = if (i < K) L(i)
      else (AL * rrecentL + BL * recentL + CL) % DL + 1
      val h = if (i < K) H(i)
      else (AH * rrecentH + BH * recentH + CH) % DH + 1
      val w = if (i < K) W(i)
      else (AW * rrecentW + BW * recentW + CW) % DW + 1

      val r = l + w

      var nl = l - 1
      var inc = 2 * (w + h)
      var dls: List[Long] = Nil
      var drs: List[Long] = Nil
      var newL = l
      var newR = r
      import scala.util.control.Breaks._
      breakable{
        do {
          rs.minAfter(nl) match {
            case Some(cr) =>
              val cl = ls.maxBefore(cr).get
              if (cl <= l) {
                if (cr >= l) {
                  if (cr < r) {
                    inc -= 2 * (h + cr - l)
                    drs ::= cr
                    newL = 0
                  } else {
                    inc -= 2 * (h + w)
                    newR = 0
                    newL = 0
                    break
                  }
                }
              } else { // cl > l
                if (cl <= r) {
                  if (cr < r) {
                    inc -= 2 * (h + cr - cl)
                    dls ::= cl
                    drs ::= cr
                  } else {
                    inc -= 2 * (h + r - cl)
                    dls ::= cl
                    newR = 0
                    break
                  }
                } else {
                   break
                }

              }
              ls.minAfter(cr) match {
                case Some(cl) => nl = cl
                case None => break
              }
            case None =>
             break
          }
        } while(true)
      }

      dls.foreach (ls.remove)
      drs.foreach (rs.remove)
      if (newL > 0) ls.add(newL)
      if (newR > 0) rs.add(newR)
      P += inc
//      print(P + " ")
      ans = if (ans == 0) P else modProd(ans, P)

      // update l r

      rrecentL = recentL
      recentL = l
      rrecentW = recentW
      recentW = w
      rrecentH = recentH
      recentH = h
    }
//    println();
    printResult(ans.toString)

    def modProd(a: Long, b: BigInt): Int = {
      val ret = ((a % m) * (b % m)) % m
      ret.toInt
    }
  }

  def init(): Unit = {

  }
}
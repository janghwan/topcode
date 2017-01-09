package hackercup.y2016.qualification

import lib.CodeJam

import scala.annotation.tailrec

/**
  * Created by essis on 2016. 1. 8..
  */
class B(fileName: String) extends CodeJam(fileName) {




  def solve(): Unit = {
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)

    val N = row(0)

    val arr = Array.ofDim[StringBuilder](2)

    arr(0) = new StringBuilder(br.readLine())
    arr(1) = new StringBuilder(br.readLine())

    var c1 = 0
    var s1, s2 = -1 // negative for not counting

    var count = 0
    while (c1 <= N) {
      if (c1 >= N || arr(0)(c1) == 'X' ) {
        // if region is counting, should finish the region.
        if (s1 >= 0) {
          //check if it's already covered
          isCovered(arr(0), s1, c1) match {
            case (true, _) =>
            case (false, true) if arr(1)(c1-1) == '.' =>
              // put a guard other side
              var i = c1 -1 -1
              while (i >= 0 && arr(1)(i) != 'X') {
                if (arr(1)(i) == '.') arr(1)(i) = '*'
                i -= 1
              }

              i = c1
              while (i < N && arr(1)(i) != 'X') {
                if (arr(1)(i) == '.') arr(1)(i) = '*'
                i += 1
              }

              arr(1)(c1 - 1) = 'G'
              count += 1
              arr(0)(c1 - 1) = '*'
            case _ =>
              // put a guard this side
              var i = s1
              while (i < N && arr(0)(i) != 'X') {
                if (arr(0)(i) == '.') arr(0)(i) = '*'
                i += 1
              }
              arr(0)(c1 - 1) = 'G'
              if (arr(1)(c1 - 1) == '.') arr(1)(c1 - 1) ='*'
              count += 1
          }
          s1 = -1
        }
      } else {
        //start of region
        if (s1 < 0) s1 = c1
      }

      if (c1 >= N || arr(1)(c1) == 'X' ) {
        // if region is counting, should finish the region.
        if (s2 >= 0) {
          //check if it's already covered
          isCovered(arr(1), s2, c1) match {
            case (true, _) =>
            case (false, true) if arr(0)(c1-1) == '.' =>
              // put a guard other side
              var i = c1 - 1 -1
              while (i >= 0 && arr(0)(i) != 'X') {
                if (arr(0)(i) == '.') arr(0)(i) = '*'
                i -= 1
              }

              i = c1
              while (i < N && arr(0)(i) != 'X') {
                if (arr(0)(i) == '.') arr(0)(i) = '*'
                i += 1
              }

              arr(0)(c1 - 1) = 'G'
              count += 1
              arr(1)(c1 - 1) = '*'
            case _ =>
              // put a guard this side
              var i = s2
              while (i < N && arr(1)(i) != 'X') {
                if (arr(1)(i) == '.') arr(1)(i) = '*'
                i += 1
              }
              arr(1)(c1 - 1) = 'G'
              if (arr(0)(c1 - 1) == '.') arr(0)(c1 - 1) ='*'
              count += 1
          }
          s2 = -1
        }
      } else {
        //start of region
        if (s2 < 0) s2 = c1
      }
      c1 += 1
    }
//    println(arr(0))
//    println(arr(1))
    printResult(count.toString)

  }


  /**
    *
    * @param str
    * @param start
    * @param end
    * @return (Boolean, Boolean)
    */
  def isCovered(str: StringBuilder, start: Int, end: Int): (Boolean, Boolean) = {
    var covered = str(end - 1) != '.'
    var otherSide = str(end - 1) == '.'
    var i = end -2
    while (i >= start) {
      if (str(i) == '.') {
        covered = false
        otherSide = false
      }
      i -= 1
    }
    (covered, otherSide)
  }

  def init(): Unit = {

  }
}
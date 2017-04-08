package hackercup.y2017.qualification

import lib.CodeJam

/**
  * Created by essis on 2016. 1. 8..
  */
class A(fileName: String) extends CodeJam(fileName) {


  /*
  x^2 + y^2 > 1 ? white : else

if p < 50
  (50, 100) cos(360 * p / 100)
   */

  case class Point(x: Double, y: Double)

  def solve(): Unit = {
    val row: Array[Long] = br.readLine.split("\\s+").map(_.toLong)
    val (p, b) = (row(0), Point(row(1), row(2)))

    val o = Point(50, 50)

    val a = Point(50, 100)

    if (dist(o, b) > 50) printResult("white")
    else {
      if (b.x < 50) {
        if (p <= 50) printResult("white")
        else {
          val ang = angle(o, a, b)
          val pang = Math.PI * 2 - 2 * Math.PI * p / 100
//          println(s"${ang / Math.PI * 180}, ${pang / Math.PI * 180}")
          if (ang > pang) printResult("black")
          else printResult("white")
        }
      } else {
        if (p > 50) printResult("black")
        else {
          val ang = angle(o, a, b)
          val pang = 2 * Math.PI * p / 100
//          println(s"${ang / Math.PI * 180}, ${pang / Math.PI * 180}")
          if (ang > pang) printResult("white")
          else printResult("black")
        }
      }
    }

    def dist(p1: Point, p2: Point) = Math.sqrt(Math.pow(p1.x-p2.x, 2) + Math.pow(p1.y-p2.y, 2))

    def angle(p1: Point, p2: Point, p3: Point) = {
      val d12 = dist(p1, p2)
      val d23 = dist(p2, p3)
      val d31 = dist(p3, p1)
      Math.acos(
        (d12 * d12 + d31 * d31 - d23 * d23) / (2 * d12 * d31)
      )
    }

  }


  def init(): Unit = {

  }
}
package hackercup.y2017.R1

import lib.CodeJam

import scala.annotation.tailrec

/**
  * Created by essis on 2016. 1. 8..
  */
class B(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)
    val (n, r) = (row(0), row(1))

    val killed = Array.ofDim[Boolean](n)
    val zombies = Array.ofDim[(Int, Int)](n)
    for (i <- 0 until n) {
      val row = br.readLine.split("\\s+").map(_.toInt)
      zombies(i) = (row(0),row(1))
    }

    @tailrec
    def findMax(i: Int, killedZombies: Array[((Int,Int), Int)], mi: Int): (Array[((Int, Int), Int)], Int) = {
      if (i >= n) (killedZombies, mi)
      else {
        val z = zombies(i)
        val box = (z._1 + r, z._2 + r) :: (z._1 + r, z._2 - r) :: (z._1 - r, z._2 + r) :: (z._1 - r, z._2 - r) :: Nil

        val killedCounts = box map { b =>
          zombies.zipWithIndex.filter({ case ((x, y), zi) =>
            !killed(zi) && isIn(b, z, (x, y))
          })
        }

        val killedCount = killedCounts.sortBy(-_.length).head

        if (killedCount.length > killedZombies.length) findMax(i+1, killedCount, i)
        else findMax(i+1, killedZombies, mi)
      }
    }

    def isIn(p1: (Int, Int), p2: (Int, Int), p: (Int, Int)): Boolean = {
      val c2 = if (p1._2 < p2._2)
        p._2 >= p1._2 && p._2 <= p2._2
      else
        p._2 <= p1._2 && p._2 >= p2._2
      val c1 = if (p1._1 < p2._1)
        p._1 >= p1._1 && p._1 <= p2._1
      else
        p._1 <= p1._1 && p._1 >= p2._1
      c1 && c2

    }

    def kill(tbk: Array[((Int, Int), Int)]) = {
      tbk foreach {
        case (_, i) => killed(i) = true
      }
    }


    val (toBeKilled, _) = findMax(0, Nil.toArray, 0)

    kill(toBeKilled)

    val (toBeKilled2, _) = findMax(0, Nil.toArray, 0)

    printResult((toBeKilled.length + toBeKilled2.length).toString)

  }


  def init(): Unit = {

  }
}
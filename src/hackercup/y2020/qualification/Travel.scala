package hackercup.y2020.qualification

import lib.CodeJam

/**
  * Created by essis on 2016. 1. 8..
  */
class Travel(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val N: Int = br.readLine.toInt
    val I: String = br.readLine
    val O: String = br.readLine

    val output = Array.ofDim[Char](N,N)

    for (i <- 0 until N; j <- 0 until N) {
      output(i)(j) =
        if (i == j) 'Y'
        else
          if (I(j) == 'Y' && O(i) == 'Y') {
            if ( Math.abs(i - j) == 1) 'Y'
            else if (j > 0 && O(j-1) == 'Y' && output(i)(j-1) == 'Y') 'Y'
            else if (i > 0 && I(i-1) == 'Y' && output(i-1)(j) =='Y') 'Y'
            else 'N'
          } else 'N'
    }

    printResult("\n" + output.map(_.mkString("")).mkString("\n"))
  }


  def init(): Unit = {

  }
}
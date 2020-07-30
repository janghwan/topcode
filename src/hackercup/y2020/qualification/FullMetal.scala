package hackercup.y2020.qualification

import lib.CodeJam

class FullMetal(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val N: Int = br.readLine.toInt
    val C: String = br.readLine

    val a = C.count(_ == 'A')
    if (Math.abs(2 * a - N) > 1) printResult("N")
    else printResult("Y")
  }


  def init(): Unit = {

  }
}
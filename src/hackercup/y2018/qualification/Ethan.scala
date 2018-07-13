package hackercup.y2018.qualification

import lib.CodeJam

/**
  * Created by essis on 2016. 1. 8..
  */
class Ethan(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val input: String = br.readLine
    var prefix = input take 1
    var pos = 0
    for (x <- input.tail) {
      if (x == input(pos)) {
        pos += 1
      } else {
        if (pos > 0) {
          printResult(prefix + input)
          return
        } else {
          prefix = prefix + x
        }
      }
    }
    printResult("Impossible")
  }


  def init(): Unit = {

  }
}
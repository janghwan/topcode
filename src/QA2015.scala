import java.io.IOException

import lib.CodeJam

/**
 * Created by essis on 15. 3. 15..
 */
class QA2015(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val row: Array[String] = br.readLine.split("\\s+")

    val Smax = row(0).toInt
    val S = row(1)
    var count = 0
    var sol = 0
    for (i <- 0 until S.length) {
      if (i > count) {
        sol += (i - count)
        count = i
      }

      count += S.charAt(i).toString.toInt
    }


    printResult(sol.toString)


  }

  def init(): Unit = {

  }
}

package codejam.y2017.qualification

import lib.CodeJam

/**
 * Created by essis on 15. 3. 15..
 */
class A(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val row = br.readLine.split("\\s+")

    var s = row(0)
    val k = row(1).toInt
    var i = 0
    var count = 0
    while (true) {
      while(i < s.length && s(i) == '+') i += 1
      if (i >= s.length) {
        if (!s.exists(_ == '-'))
          printResult(count.toString)
        else
          printResult("IMPOSSIBLE")
        return
      }
      s= flip(s, i)
//      println(s)
      count += 1
      if (!s.exists(_ == '-')) {
        printResult(count.toString)
        return
      }
    }

    def flip(s: String, i: Int) = {
      val ii = if (i < s.length - k) i else s.length - k
      (if (ii > 0) s.substring(0, ii) else "") + reverse(s.substring(ii, ii + k)) + s.substring(ii + k)
    }

    def reverse(s: String) = s map {
      case '+' => '-'
      case '-' => '+'
    }



  }

  def init(): Unit = {

  }
}

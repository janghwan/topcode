import lib.CodeJam

import scala.annotation.tailrec


/**
 * Created by essis on 15. 3. 15..
 */
class R1B2015A(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val Ns: String = br.readLine.trim
    val N: Long = Ns.toLong
    val digits = (N - 1).toString.length
    digits match {
      case 1 =>
        printResult(N.toString)
      case d =>

        printResult((part(N) + lenDis(digits)).toString)


    }

//    printResult(sol.toString)
  }

  def part(n: Long): Long = {
    val digits = n.toString.length
    val div = digits / 2 + digits % 2
    val head = n.toString.substring(0, div).toLong
    val tail = n.toString.substring(div).toLong
    val hnine = if (div > 1) new String(Array.fill(div - 1)('9')).toLong else 0 // 9999
    if (head / ( hnine + 1) == 1 && head % ( hnine + 1)==0) { // head = 100000
      if (tail == 0) {
        val nine = new String(Array.fill(digits-1)('9')).toLong // 9999
        val div = (digits-1) / 2 + (digits-1) % 2
        val head = nine.toString.substring(0, div).toLong
        val tail = nine.toString.substring(div).toLong
        head + tail -1
      } else { // 100021
        tail - 1
      }
    } else {
      if (tail == 0) {
        val nine2 = new String(Array.fill(digits - div)('9')).toLong
        (head - 1).toString.reverse.toLong - 1 + nine2 - 1 + 1
      } else {
        head.toString.reverse.toLong - 1 + tail - 1 + 1
      }

    }

  }

  def lenDis(n: Int): Long = {
    n match {
      case 2 =>
        11
      case _ =>

        lenDis(n-1) + part(new String(Array.fill(n-1)('9')).toLong) + 2
    }

  }

  def init(): Unit = {

  }
}

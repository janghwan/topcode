import java.math.MathContext

import scala.annotation.tailrec

class QA2016(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val N = BigInt(br.readLine.toInt)
    val table: Array[Boolean] = Array.ofDim(10)

    run(1)
    @tailrec
    def run(count: Int): Unit = {
      if (count > 1000000) {
        printResult("INSOMNIA")
        return
      }
      (N * count).toString.foreach {
        i =>
          table(i.toString.toInt) = true
      }

      if (table.forall(x=>x)) {
        printResult(N * count toString)
      } else {
        run(count + 1)
      }
    }
  }

  def init(): Unit = {

  }
}
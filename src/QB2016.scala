import scala.annotation.tailrec

class QB2016(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val p = br.readLine()
    var count = 0
    var prev = p(0)
    p.foreach {
      x =>
        if (x == prev) {

        } else {
          count += 1
          prev = x
        }
    }

    if (prev == '-') {
      count += 1
    }

    printResult(count toString)
  }

  def init(): Unit = {

  }
}
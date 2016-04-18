

class R1AB2016(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val N = br.readLine.toInt

    val count: Array[Int] = Array.ofDim(2500)

    for (i <- 0 until 2*N-1) {
      val line = br.readLine().split("\\s+").map(_.toInt)
      line foreach {
        n => count(n-1) = count(n-1) + 1
      }
    }

    val out = new StringBuilder

    for (i <- 0 until 2500) {
      if (count(i) % 2 == 1) {
        out ++= " " + (i+1).toString
      }
    }

    printResult(out.toString().trim)
  }

  def init(): Unit = {

  }
}
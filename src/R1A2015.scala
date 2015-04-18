

/**
 * Created by essis on 15. 3. 15..
 */
class R1A2015(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val N = br.readLine.trim.toInt
    val ar: Array[Int] = br.readLine.split("\\s+").map(_.toInt)



    var sol1 = 0
    for (i <- 0 until N - 1) {
      if (ar(i) > ar(i+1)) {
        sol1 += (ar(i) - ar(i+1))
      }
    }
    var sol2 = 0

    var rate = 0
    for (i <- 0 until N - 1) {
      rate = Math.max(rate, ar(i) - ar(i+1))
    }

    for (i <- 0 until N - 1) {
      val gap = (if (ar(i) < rate) ar(i) else rate)
      sol2 += gap
    }

    printResult(sol1 + " " + sol2)

  }

  def init(): Unit = {

  }
}

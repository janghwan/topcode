import java.math.MathContext

/**
 * Created by essis on 15. 3. 15..
 */
class RAAPAC16B(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)
    val N = row(0)
    val M = row(1)

    val a = br.readLine().split("\\s+").map(_.toInt)
    var temp = BigDecimal(1)
    val acc = a map {
      x =>
        temp = temp * x
        temp
    }

    val answer = new StringBuilder
    for (c <- 0 until M) {
      val row = br.readLine.split("\\s+").map(_.toInt)
      val L = row(0)
      val R = row(1)
      val D = R - L + 1
      val vol = if (L > 0) {
        acc(R) / acc(L-1)
      } else {
        acc(R)
      }

      val exp = vol.round(MathContext.DECIMAL128).toString.length - vol.scale - 1
      println(exp * Math.log10((vol / Math.pow(exp, 10)) toDouble) / D)
      val sol = BigDecimal(Math.pow(10, exp + Math.log10(vol / exp toDouble) / D)).setScale(8, BigDecimal.RoundingMode.HALF_UP)
      answer.append("\n" + sol.toString)
    }

    printResult(answer.toString())

  }

  def init(): Unit = {

  }
}
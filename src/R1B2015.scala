import scala.annotation.tailrec


/**
 * Created by essis on 15. 3. 15..
 */
class R1B2015(fileName: String) extends CodeJam(fileName) {

  def GCD(a: BigInt, b: BigInt): BigInt = {
    if (b == BigInt(0)) a
    else GCD(b, a % b)
  }

  def LCM(a: BigInt, b: BigInt): BigInt = {
    (a * b) / GCD(a,b)
  }

  @tailrec
  private def bsearch2(a: BigInt, b: BigInt, target: BigInt, ar: Array[BigInt]): Int = {

    val center = (a + b) / 2
    val count = ar.fold(BigInt(0))((x,y) => if (center % y == BigInt(0) ) x + center / y  else  x + center / y + 1)
    var c = 0
    for (i <- ar.indices) {
      if (center % ar(i) == BigInt(0)) {
        c += 1
        if (count + c == target) {
          return i

        }
      }
    }

   if (count + c >= target) {
      bsearch2(a, center, target, ar)
    } else {
      bsearch2(center+1, b, target, ar)
    }
  }


  def solve(): Unit = {
    val row: Array[BigInt] = br.readLine.split("\\s+").map(BigInt(_))
    val B = row(0).toInt
    val N = row(1)
    val ar: Array[BigInt] = br.readLine.split("\\s+").map(BigInt(_))
    val lcm = ar.reduce(LCM)
    val batch = ar.fold(BigInt(0))((a,b) => a + (lcm / b))
    if (N % batch == BigInt(0)) {
      var m = lcm
      var sol = 0
      for (i <- ar.indices) {
        if ((lcm - 1)% ar(i) <= m) {
          m = (lcm - 1) % ar(i)
          sol = i
        }
      }
      printResult(s"${sol+1}")
      return
    }
    val sol = bsearch2(0, lcm, N % batch, ar) + 1
    printResult(sol.toString)
  }

  def init(): Unit = {

  }
}

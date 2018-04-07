package codejam.y2017.qualification

import lib.CodeJam

/**
 * Created by essis on 15. 3. 15..
 */
class C(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val row = br.readLine.split("\\s+").map(_.toLong)
    val N = row(0)
    val K = row(1)

    def sol(n: Long, k: Long): (Long, Long) = {
//      println(s"$n, $k")
      (n, k) match {
        case (nn, 1) =>
          if (nn % 2 == 1) (nn / 2, nn / 2)
          else (nn / 2, nn / 2 - 1)
        case (nn, kk) =>
          if (nn % 2 == 1)
            if (kk % 2 == 1)
              sol(nn / 2, kk / 2)
            else
              sol(nn / 2, kk / 2)
          else
            if (kk % 2 == 1)
              sol(nn / 2 - 1, (kk - 1) / 2)
            else
              sol(nn / 2, (kk -1) / 2 + 1)
      }
    }

    val (x, y) = sol(N, K)

    printResult(s"$x $y")

  }

  def init(): Unit = {

  }
}

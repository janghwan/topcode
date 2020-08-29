package hackercup.y2020.round2

import lib.CodeJam

class Elimination(fileName: String) extends CodeJam(fileName) {
  // NOT WORKING
  def solve(): Unit = {
    val N = sc.nextInt()
    val P = sc.nextDouble()
    sc.nextLine()

    val res = new StringBuilder()

    for (j <- 0 until N) {
      var ans: Double = 1.0
      var pWin: Double = P
      var nFront: Double = j
      var nBack: Double = N - 1 - j
      var pSelect: Double = 2.0 / N
      var pSurvive: Double = 1.0
      for (i <- 1 until N-1) {
        pWin = nFront/(nFront + nBack) * P + nBack/(nFront + nBack)*(1-P)
        pSurvive = pSurvive * (pSelect * pWin + (1 - pSelect))
        ans += pSurvive
        val pFront = nFront / (nFront + nBack)
        val pFFront = pFront * (nFront - 1) / (nFront - 1 + nBack)
        val pBack = nBack / (nFront + nBack)
        val pBBack = pBack * (nBack - 1) / (nFront + nBack - 1)
        val pBF = 1 - pBBack - pFFront
        nFront = pSelect * ((nFront - 1) * pFront + nFront * pBack) + (1 - pSelect) * ((nFront - 2) * pFFront  + (nFront - 1) * pBF + nFront * pBBack)
        nBack = pSelect * ((nBack - 1) * pBack + nBack * pFront) + (1 - pSelect) * ((nBack - 2) * pBBack + (nBack - 1) * pBF + nBack * pFFront)
        pSelect = 2.0 / (N  - i)
      }
      res.append("\n" + ans)
    }
    printResult(res.toString)
  }


  def init(): Unit = {

  }
}
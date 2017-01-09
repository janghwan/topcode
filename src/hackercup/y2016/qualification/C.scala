package hackercup.y2016.qualification

import lib.CodeJam

/**
  * Created by essis on 2016. 1. 8..
  */
class C(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)
    val N = row(0)
    val P = row(1)

    val arr: Array[Long] = br.readLine().split("\\s+").map(_.toLong)

    if (N == 1) {
      if (arr(0) <= P) {
        printResult("1")
      } else {
        printResult("0")
      }
      return
    }

    val sumArr: Array[Long] = Array.ofDim[Long](N + 1)

    sumArr(0) = 0
    for (i <- 1 to N) {
      sumArr(i) = sumArr(i-1) + arr(i - 1)
    }

    var count = 0L
    var i= 0
    var j = 1

    while (i < N && j <= N){

      while(j < N && sumArr(j+1) - sumArr(i) <= P) {
        j+=1
      }

      if (sumArr(j) - sumArr(i) <= P) {
        count += (j - i) * (j - i - 1) / 2
        j+=1
      }
      i+=1

      /*if (i == 0)  { if (sumArr(j) <= P) {count += 1; println(s"$i, $j")} }
      else if (sumArr(j) - sumArr(i-1) <= P) {count += 1; println(s"$i, $j")}*/
    }

    for (i <- arr) {
      if (i <= P) count += 1
    }

    printResult(count.toString)

  }

  def init(): Unit = {

  }
}
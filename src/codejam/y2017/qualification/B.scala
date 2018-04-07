package codejam.y2017.qualification

import lib.CodeJam

/**
 * Created by essis on 15. 3. 15..
 */
class B(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    var row = br.readLine.map(_.toString.toInt)

    var i = row.length - 1
    while(true) {
      while(i > 0 && row(i) >= row(i-1)) i-=1
      if (i==0) {
        printResult(row.map(_.toString).mkString(""))
        return
      }
      row = set9(row, i)
//      println(row.mkString(""))
    }

    def set9(row: Seq[Int], i: Int) = {
      val temp = row.slice(0, i) ++ List.fill(row.length - i)(9)
      val temp1 = temp.mkString("").toLong - (1 :: List.fill(row.length - i)(0)).mkString("").toLong
      temp1.toString.map(_.toString.toInt)
    }


  }

  def init(): Unit = {

  }
}

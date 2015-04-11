import scala.annotation.tailrec

/**
 * Created by essis on 15. 3. 15..
 */
class QB2015(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val D = br.readLine.toInt
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)

    var max = 0
    var maxi = 0
    var sol = Int.MaxValue

/*    var n = 0
    do {
      max = 0
      for (i <- row.indices) {
        if (row(i)> max) {
          max = row(i)
          maxi = i
        }
      }

      if (sol > max + n)
        sol = max + n
      row(maxi) = (row(maxi) / 2) + (row(maxi) % 2)

      n += 1

    } while (max > 3)*/

    val max1 = row.fold(0)((a,b) => if (a>b) a else b)


    val sol2 = Math.ceil(Math.log(max1) / Math.log(2)).toInt + 1

    def time(list: List[Int], step: Int, sol: Int): Int = {
      val max = list.fold(0)((a,b) => if (a>b) a else b)
      val ret = if (max + step > sol) sol else max + step
      if (max < 4) return ret
      val (left, right) = list.span(_ != max)
      val broken = (left ::: List(max / 2, (max / 2) + (max % 2)) ::: right.tail).filter(_>0)
      time(broken, step + 1, ret)
    }

    println(row.toList.toString)
    printResult(sol2.toString)


  }

  def init(): Unit = {

  }
}

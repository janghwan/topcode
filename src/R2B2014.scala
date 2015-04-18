import java.io.IOException

/**
 * Created by essis on 15. 3. 15..
 */
class R2B2014(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    br.readLine
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)
    var start = 0
    var end = row.length - 1

    var move = 0
    while (start < end) {
      var mi = min(row, start, end)

      if (mi - start < end - mi) {
        while (mi > start) {
          val temp = row(mi - 1)
          row(mi - 1) = row(mi)
          row(mi) = temp
          move += 1
          mi-=1
        }
        start += 1
      } else {
        while (mi < end) {
          val temp = row(mi + 1)
          row(mi + 1) = row(mi)
          row(mi) = temp
          move += 1
          mi+=1
        }
        end -= 1
      }
    }

    printResult(move.toString)

    def min(arr: Array[Int], start: Int, end: Int): Int = {
      var min = Int.MaxValue
      var index = 0
      for (i <- start to end) {
        if (arr(i) < min) {
          min = arr(i)
          index = i
        }
      }
      index
    }

  }

  def init(): Unit = {

  }
}

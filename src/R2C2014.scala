
/**
 * Created by essis on 15. 3. 15..
 */
class R2C2014(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)
    val W = row(0)
    val H = row(1)
    val B = row(2)


    val dist = Array.ofDim[Int](B)
    val checked = Array.ofDim[Boolean](B)
    val buildings = Array.ofDim[((Int, Int), (Int, Int))](B)

    def distance(a: ((Int, Int), (Int, Int)), b: ((Int, Int), (Int, Int))) = {
      val dx = if (a._1._1 > b._2._1) {
        a._1._1 - b._2._1 -1
      } else if (a._2._1 < b._1._1) {
        b._1._1 - a._2._1 - 1
      } else 0

      val dy = if (a._1._2 > b._2._2) {
        a._1._2 - b._2._2 - 1
      } else if (a._2._2 < b._1._2) {
        b._1._2 - a._2._2 - 1
      } else 0

      Math.max(dx, dy)
    }


    def min = {
      var min = Int.MaxValue
      var index = -1
      for (i <- dist.indices) {
        if (!checked(i) && dist(i) < min) {
          min = dist(i)
          index = i
        }
      }
      index
    }

    for (
      i <- 0 until B
    ) {
      val b = br.readLine.split("\\s+").map(_.toInt)
      buildings(i) = ((b(0), b(1)), (b(2), b(3)))
      //distance from start
      dist(i) = buildings(i)._1._1
    }

    var m = min

    while (m >= 0) {
      for (i <- dist.indices) {
        if ( i != m ) {
          dist(i) = Math.min(
            distance(buildings(m), buildings(i)) + dist(m),
            dist(i)
          )
        }
      }
      checked(m) = true
      m = min
    }

    //finding distance from the left

    for (i <- dist.indices) {
      dist(i) = dist(i) + (W - buildings(i)._2._1 - 1)
    }

    val sol = Math.min(dist.fold(Int.MaxValue)(Math.min), W)
    printResult(sol.toString)
  }

  def init(): Unit = {

  }
}

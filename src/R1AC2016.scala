import scala.collection.mutable

class R1AC2016(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
    val N = br.readLine.toInt

    var paths = Set[mutable.DoubleLinkedList[Int]]()

    val in = br.readLine().split("\\s+").map(_.toInt)

    val mat = Array.ofDim[Int](N,N)


    for (i <- 0 until N) {
      mat(i)(in(i)-1) = 1
    }

    for (i <- 0 until N) {
      // check if this is included in prev paths
      if (!paths.exists(_.contains(i))) {
        var list = mutable.DoubleLinkedList[Int](i)
        var x = i
        while (!list.contains(in(i))) {
          list = list :+ x
          x = in(x)
        }

        paths = paths + list
      }
    }

      printResult(out.toString().trim)
  }

  def init(): Unit = {

  }
}
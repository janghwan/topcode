import java.io.IOException

/**
 * Created by essis on 15. 3. 15..
 */
class R2C2014(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {
   /* br.readLine
    val row: Array[Int] = br.readLine.split("\\s+").map(_.toInt)
    val W = row(0)
    val H = row(1)
    val B = row(2)


    val mat = Array.ofDim[(Int, Boolean, Boolean)](W, H)

    for (
      i <- 0 until W;
      j <- 0 until H
    ) {
      mat(i)(j) = (0, false, false)
    }

    for (
      i <- 0 until B
    ) {
      val b = br.readLine.split("\\s+").map(_.toInt)
      for (
        j <- b(0) to b(2);
        k <- b(1) to b(3)
      ) {
        mat(j)(k) = mat(j)(k).copy(_2 = true)
      }
    }

    var ans = 0

    //initial flow
    for(
      x <- 0 until W;
      //y <- 0 until H
    ) {
      if (!mat(x)(0)._2) {
        mat(x)(0) = mat(x)(0).copy(_1 = 1, _3 = true)
      }
    }
    for(
      x <- 0 until W;
      y <- 0 until H
    ) {
      if (y + 1 < W) {
        if (mat(x)(y)._1 > 0) {
          if (mat(x)(y+1)._1 == 0 && !mat(x)(y+1)._2) {
            mat(x)(y+1) = mat(x)(y+1).copy(_1 = 1)
          } else if (x - 1 >= 0 && mat(x-1)(y)._1 == 0 && !mat(x-1)(y)._2) {
            mat(x-1)(y) = mat(x-1)(y).copy(_1 = 1)
          } else if (x + 1 < W && mat(x+1)(y)._1 == 0 && !mat(x+1)(y)._2) {
            mat(x+1)(y) = mat(x+1)(y).copy(_1 = 1)
          }
        }

      }
    }*/
  }

  def init(): Unit = {

  }
}

object R2C2014 {
  def main(args: Array[String]) {
    var fileName: String = "A-"
    if (args(0).compareTo("small") == 0) {
      fileName += "small"
    }
    else if (args(0).compareTo("large") == 0) {
      fileName += "large"
    }
    else if (args(0).compareTo("sample") == 0) {
      fileName += "sample"
    }
    else {
      System.out.println("small or large or sample")
      return
    }
    if (args(1).compareTo("1") == 0) {
      fileName += "-practice.in.txt"
    }
    else {
      fileName += ".in.txt"
    }
    try {
      val codeJam: R2C2014 = new R2C2014(fileName)
      codeJam.run
    }
    catch {
      case e: IOException => {
        System.err.print(e)
      }
    }
  }
}

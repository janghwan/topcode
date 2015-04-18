import scala.annotation.tailrec

/**
 * Created by essis on 15. 3. 15..
 */
class R1B2015(fileName: String) extends CodeJam(fileName) {

  def GCD(a: Long, b: Long): Long = {
    var tmp = 0L
    var bb = b
    var aa = a
    while(bb > 0) {
      tmp = aa
      aa = bb
      bb = tmp % bb
    }

    aa
  }

  private def bsearch2(a: Long, b: Long, target: Long, ar: Array[Long]): Int = {
    if (a == b) {
      val count = ar.fold(0L)((x,y) => if (a % y == 0 ) x + a / y else  x + a / y + 1)
      val t = target - count
      var c = 0
      for (i <- ar.indices) {
        if (a % ar(i) == 0) {
          c += 1
          if (c==t)
            return i
        }
      }
      throw new Exception("error3")
    }


    val center = (a + b) / 2
    val count = ar.fold(0L)((x,y) => x + center / y + 1)

//    println(s"a $a, b $b, center $center, count $count, target $target")
    if (count == target) {
      var sol = 0
      for (i <- ar.indices) {
        if (center % ar(i) == 0) {
          sol = i
        }
      }
      sol
    } else if (count > target) {
      bsearch2(a, center, target, ar)
    } else {
      bsearch2(center+1, b, target, ar)
    }
  }

  /*private def bsearch(a: Long, b: Long, target: Long, ar: Array[Long]): Int = {
    if (a == b) {
      val count = ar.fold(0L)((x,y) => if (a % y == 0 ) x + a / y else  x + a / y + 1)
      val t = target - count
      var c = 0
      for (i <- ar.indices) {
        if (a % ar(i) == 0) {
          c += 1
          if (c==t)
            return i
        }
      }
      throw new Exception("error3")
    }

    if (a == 0 && a + 1 == b) {
      var c = 0
      for (i <- ar.indices) {
        if (a % ar(i) == 0) {
          c += 1
          if (c==target)
            return i
        }
      }
      throw new Exception("error4")

    }

    val center = (a + b) / 2
    val count = ar.fold(0L)((x,y) => x + center / y + 1)
    val countd = ar.fold(0L)((x,y) => x + (center - 1) / y + 1)

    println(s"a $a, b $b, center $center, count $count, countd $countd, target $target")
    if (count == target) {
      var sol = 0
      var min = Long.MaxValue
      for (i <- ar.indices) {
        if (center % ar(i) <= min) {
          min = center % ar(i)
          sol = i
        }
      }
      sol
    } else if (countd == target) {
      var sol = 0
      var min = Long.MaxValue
      for (i <- ar.indices) {
        if ((center-1) % ar(i) <= min) {
          min = (center-1) % ar(i)
          sol = i
        }
      }
      sol
    } else if( countd < target && count > target) {
      val t = target - countd
      var c = 0
      for (i <- ar.indices) {
        if (center % ar(i) == 0) {
          c += 1
          if (c==t)
            return i
        }
      }
      throw new Exception("error2")
    } else if (count > target) {
      bsearch(a, center, target, ar)
    } else {
      bsearch(center, b, target, ar)
    }

  }*/


  def solve(): Unit = {
    val row: Array[Long] = br.readLine.split("\\s+").map(_.toLong)
    val B = row(0).toInt
    val N = row(1)
    val ar: Array[Long] = br.readLine.split("\\s+").map(_.toLong)
    val gcd = ar.reduce(GCD)
    val lcm = ar.reduce((a, b) => (a * b) / gcd)
    val batch = ar.fold(0L)((a,b) => a + (lcm / b))
    println(s"lcm $lcm, batch $batch")
    if (N % batch == 0) {
      printResult(s"$B")
      return
    }
    val sol = bsearch2(0, lcm, N % batch, ar) + 1
    printResult(sol.toString)
  }

  def init(): Unit = {

  }
}

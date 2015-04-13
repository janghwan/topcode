

/**
 * Created by essis on 15. 3. 15..
 */
class QC2015(fileName: String) extends CodeJam(fileName) {
  sealed class L

  case class O(sign: Int) extends L
  case class I(sign: Int) extends L
  case class J(sign: Int) extends L
  case class K(sign: Int) extends L

  def mapping(c: Char): L = {
    c match {
      case 'i' => I(1)
      case 'j' => J(1)
      case 'k' => K(1)
    }
  }

  def mult(a: L, b: L): L = {
    (a, b) match {
      case (O(x), O(y)) => O(x * y)
      case (O(x), I(y)) => I(x * y)
      case (O(x), J(y)) => J(x * y)
      case (O(x), K(y)) => K(x * y)
      case (I(x), O(y)) => I(x * y)
      case (I(x), I(y)) => O(- x * y)
      case (I(x), J(y)) => K(x * y)
      case (I(x), K(y)) => J(- x * y)
      case (J(x), O(y)) => J(x * y)
      case (J(x), I(y)) => K(- x * y)
      case (J(x), J(y)) => O(- x * y)
      case (J(x), K(y)) => I(x * y)
      case (K(x), O(y)) => K(x * y)
      case (K(x), I(y)) => J(x * y)
      case (K(x), J(y)) => I(- x * y)
      case (K(x), K(y)) => O(- x * y)

    }


  }


  def solve(): Unit = {
    val row: Array[Long] = br.readLine.split("\\s+").map(_.toLong)

    val LL = row(0).toInt
    val X = row(1)
    val line = br.readLine().map(mapping).toArray

    val x = if (X > 4) 4 else X.toInt
    val multF: Array[L] = Array.ofDim(x * LL)
    val multR: Array[L] = Array.ofDim(x * LL)

    multF(0) = line.head

    for (i <- 0 until multF.length - 1) {
      multF(i+1) = mult(multF(i), line((i + 1) % LL))
    }

    multR(multR.length - 1) = line(line.length-1)

    for (i <- (multF.length - 1).until(0,-1)) {
      multR(i - 1) = mult(line((i - 1) % LL), multR(i))
    }

    for (i <- multF.indices) {
      multF(i) match {
        case I(1) =>
          var jIndex = i + 1
          var j = line(jIndex % line.length)

          while (jIndex - i < multF.length && jIndex < (X.toLong * LL.toLong) - 1) {
            val kIndex = if (jIndex + 1 >= (X.toLong * LL.toLong) % multF.length) {
              ((jIndex + 1 - ((X.toLong * LL.toLong) % multF.length)) % multF.length).toInt
            } else {
              ((jIndex + 1 - ((X.toLong * LL.toLong) % multF.length)) + multF.length).toInt
            }

            val k = multR(kIndex % multF.length)
            if (j == J(1) && k == K(1)) {
              println(s"$i, $jIndex")
              printResult("YES")
              return
            }
            jIndex += 1
            j = mult(j, line(jIndex % line.length))
          }
        case _ =>

      }
    }

    printResult("NO")




  }

  def init(): Unit = {

  }
}

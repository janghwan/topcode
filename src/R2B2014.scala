import java.io.IOException

/**
 * Created by essis on 15. 3. 15..
 */
class R2B2014(fileName: String) extends CodeJam(fileName) {
  def solve(): Unit = {

  }

  def init(): Unit = {

  }

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
      val codeJam: R1B2013b = new R1B2013b(fileName)
      codeJam.run
    }
    catch {
      case e: IOException => {
        System.err.print(e)
      }
    }
  }
}

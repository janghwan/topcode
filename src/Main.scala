import java.io.IOException

import codejam.y2017.qualification._
import hackercup.y2020.qualification._
import hackercup.y2020.round2._
import lib.CodeJam

/**
 * @author Janghwan Lee <jlee@sazze.com>
 *         Sazze, Inc. 2015
 * @since 4/10/15 4:39 PM
 */
object Main {
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
      val codeJam: CodeJam = new Capastaty(fileName)
      codeJam.run
    }
    catch {
      case e: IOException => {
        System.err.print(e)
      }
    }
  }
}

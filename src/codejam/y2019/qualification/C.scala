
import scala.io.StdIn

object C {
  def main(args: Array[String]) {
    val N = StdIn.readInt()
    for (n <- 1 to N) {
      println(s"Case #$n: ${solve}")
    }
  }

  def solve = {
    StdIn.readLine()
    val a:: b:: l = StdIn.readLine().split("\\s+").map( s => BigInt(s) ).toList
    val encoded =  (a::b::l).foldLeft(List(triple(a,b)))(fact).reverse
    val primes = encoded.distinct.sorted
    val alphabet:Map[BigInt, Char] = primes.zip("ABCDEFGHIJKLMNOPQRSTUVWXYZ").toMap
    encoded.map(alphabet).mkString
  }

  def fact(l: List[BigInt], i: BigInt): List[BigInt] = (l, i) match {
    case (x::xs, a) => a / x  :: x :: xs
  }

  def triple(a: BigInt, b: BigInt) = {
    val d = if (a>b) gcd(a,b) else gcd(b,a)
    a/d
  }

  def gcd(a: BigInt,b: BigInt): BigInt = {
    if(b ==0) a else gcd(b, a%b)
  }
}
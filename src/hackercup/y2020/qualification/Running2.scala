package hackercup.y2020.qualification

import lib.CodeJam

// NOT WORKING
class Running2(fileName: String) extends CodeJam(fileName) {

  def solve(): Unit = {
    val arr = br.readLine.split(" ").map(_.toInt)
    val N = arr(0)
    val M = arr(1)
    val A = arr(2) - 1
    val B = arr(3) - 1

    val edges = Array.fill[List[Int]](N)(Nil)
    val C = Array.ofDim[Long](N)
    val P = Array.ofDim[Int](N)

    // build graph
    for (i <- 0 until N) {
      val in = br.readLine.split(" ").map(_.toInt)
      val p = in(0) - 1
      if (p >= 0) {
        P(i) = p
        edges(i) = p :: edges(i)
        edges(p) = i :: edges(p)
      }
      C(i) = in(1)
    }

    println("edges " + edges.toList)
    // find path.
    var a = A
    var path: List[Int] = Nil
    while (a != -1) {
      var b = B
      var revPath = b :: Nil
      while(b != a && b != 0) {
        b = P(b)
        revPath = b :: revPath
      }
      if (a == b) {
        println("met " + a)
        a = -1
        path = path.reverse ++ revPath
      } else {
        path = a :: path
        a = P(a)
      }
    }


    println(path.toList)
    // segment tree

    def update(tree: Array[Long], index: Int, newValue: Long) =
      updateHelp(tree, index, newValue, 1, 0, tree.length/2 - 1)

    def updateHelp(tree: Array[Long], index: Int, newValue: Long, i: Int, cl: Int, cr: Int): Unit = {
      //println(s"$index $newValue $i $cl $cr")
      val mid = (cl + cr) / 2
      if (cl<= index && cr >= index) {
        if (tree(i) > newValue) tree(i) = newValue
        if (cl != cr) {
          updateHelp(tree, index, newValue, i * 2, cl, mid)
          updateHelp(tree, index, newValue, i * 2 + 1, mid + 1, cr)
        }
      }
    }

    def min(tree: Array[Long], left: Int, right: Int): Long =
      minHelp(tree, 1, left, right, 0, tree.length/2-1)

    def minHelp(tree: Array[Long], i: Int, left: Int, right: Int, cl: Int, cr: Int): Long = {
      val mid = (cl + cr) / 2 // 0 1
      if (cl >= left && cr <= right)
        tree(i)
      else if (cl > right || cr < left)
        Long.MaxValue
      else {
        if (cl != cr)
          Math.min(
            minHelp(tree, i * 2, left, right, cl, mid),
            minHelp(tree, i * 2 + 1, left, right, mid + 1, cr)
          )
        else
          Long.MaxValue
      }
    }

    def build(C: Array[Long]): Array[Long] = {
      val segTree = Array.ofDim[Long](C.length * 2)
      buildHelp(segTree, C, 0, C.length - 1, 1)
      segTree
    }

    def buildHelp(tree: Array[Long], C: Array[Long], l: Int, r: Int, i: Int): Long = {
      if (l == r) {
        tree(i) = C(l)
        C(l)
      } else {
        val mid = (l + r) / 2
        tree(i) = Math.min(
          buildHelp(tree, C, l, mid, i * 2),
          buildHelp(tree, C, mid + 1, r, i * 2 + 1)
        )
        tree(i)
      }
    }


    val c = Array.fill(path.length)(Long.MaxValue)
    c(0) = 0
    val segTree = build(c)

    println(segTree.toList)
    // search min.
    for ((n, i) <- path.zipWithIndex.tail) {
      // for all node j distance with d from node n
      val cand = neighbors(n, M)
      for (d <- 1 until M) {
        cand(d).foreach(j => {
          val m = min(segTree, i - (M - d), i - 1)
          if (m < Long.MaxValue) {
            c(i - d) = m + C(j)
            // can be optimized only when it's different.
            update(segTree, i - d, c(i - d))
          }
        })
      }

      val m = min(segTree, i - M, i - 1)
//      println(s"min1 n $n i $i m $m l ${i - M} r ${i - 1}")
      if (C(n) != 0) {
        c(i) = m + C(n)
        update(segTree, i, c(i))
      }
      println("segTree: " + segTree.toList)
    }

    // distance -> nodes
    def neighbors(node: Int, M: Int): Array[Set[Int]] = {
      val ret = Array.fill[Set[Int]](M)(Set())
      def dfs(n: Int, depth: Int, prev: Int): Unit = {
        if (depth + 1 < M)
          for (e <- edges(n)) {
            if (e != prev) {
              if (C(e) != 0)
                ret(depth + 1) += e
              dfs(e, depth + 1, n)
            }
          }
      }
      dfs(node, 0, -1)
      ret
    }

    val ans = min(segTree, path.length - 1 - M, path.length - 2)
    if (ans == Long.MaxValue) printResult("-1")
    else printResult(ans.toString)
  }


  def init(): Unit = {

  }
}
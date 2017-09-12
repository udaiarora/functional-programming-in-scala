// 2.1
def fib(n: Int): Int = {

  @annotation.tailrec
  def fibHelper(last:Int, secondToLast: Int, n: Int): Int = {
    if(n==0) secondToLast
    else if(n==1) last
    else fibHelper(last+secondToLast, last, n-1)
  }

  fibHelper(1, 0, n)
}

//println(fib(10))



// 2.2
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

  // Checks if n and n+1 are sorted
  @annotation.tailrec
  def isSortedHelper(n: Int): Boolean = {
    if (n == as.length - 1) true
    else {
      val ord = (ordered(as(n),as(n+1)))
      if(!ord) false else isSortedHelper(n+1)
    }
  }

  isSortedHelper(0)
}
println(isSorted(Array(10,20,40,50), (one: Int, two: Int) => two>=one))
println(isSorted(Array("aa", "bb", "ccc", "ccd"), (one: String, two: String) => two.compareTo(one)>=0))
println(isSorted(Array(10,20,60,50), (one: Int, two: Int) => two>one))
println(isSorted(Array("aa", "bb", "cce", "ccd"), (one: String, two: String) => two.compareTo(one)>=0))
object IsSorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, acc: Boolean): Boolean = {
      if ((n + 1) >= as.length)
        acc
      else
        loop(n + 1, acc && ordered(as(n), as(n + 1)))
    }

    loop(0, true)
  }

  def main(args: Array[String]): Unit = {
    val msg = "Is the list sorted? %b"
    val result = isSorted(Array(5,1,2,3), (x: Int, y: Int) => x <= y)
    println(msg.format(result))
  }
}

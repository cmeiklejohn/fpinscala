object Fibonacci {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(x: Int, y: Int, acc: Int): Int = {
      if (x == 0)
        acc
      else if (x == 1)
        y
      else
        loop(x - 1, y + acc, y)
    }

    loop(n, 1, 0)
  }

  private def format(x: Int) = {
    val msg = "The fibonacci of %d is %d"
    msg.format(x, fib(x))
  }

  def main(args: Array[String]): Unit = {
    println(format(10))
  }
}

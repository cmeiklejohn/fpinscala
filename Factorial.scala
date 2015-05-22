object Factorial {
  def fact(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int = {
      if (n <= 0)
        acc
      else
        loop(n - 1, n * acc)
    }

    loop(n, 1)
  }

  private def format(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, fact(x))
  }

  def main(args: Array[String]): Unit = {
    println(format(10))
  }
}

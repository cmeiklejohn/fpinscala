sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

trait Options[+A] {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a match {
      case None => None
      case Some(x) => {
        b match {
          case None => None
          case Some(y) => Some(f(x, y))
        }
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def loop(a: List[Option[A]], b: Option[List[A]]): Option[List[A]] = {
      a match {
        case Nil => b
        case x :: xs => x match {
            case None => loop(xs, b)
            case Some(y) => b match {
              case None => Some(List(y))
              case Some(Nil) => None
              case Some(a :: as) => Some(y :: a :: as)
            }
        }
      }
    }

    loop(a, None)
  }
}

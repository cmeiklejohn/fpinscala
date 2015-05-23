package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List [A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(x, xs)
    case Cons(_, xs) => Cons(x, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], n: Int): List[A] = {
      if (n == 0)
        l
      else {
        l match {
          case Nil => Nil
          case Cons(x, xs) => loop(xs, n - 1)
        }
      }
    }

    loop(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], nl: List[A]): List[A] = {
      l match {
        case Nil => nl
        case Cons(x, xs) => {
          if (f(x) == true)
            loop(xs, Cons(x, nl))
          else
            loop(xs, nl)
        }
      }
    }

    loop(l, Nil)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => b + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
    }

}

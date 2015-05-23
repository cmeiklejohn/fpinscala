package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    def loop(t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + loop(left) + loop(right)
      }
    }

    loop(tree)
  }
}

package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  val t1: Tree[Int] =
    Branch(
      Leaf(1),
      Branch(
        Leaf(2),
        Leaf(3)))

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  object ViaFold {
    import Function.const

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
      t match {
        case Leaf(v) => f(v)
        case Branch(l, r) =>
          g(
            fold(l)(f)(g),
            fold(r)(f)(g))
      }

    def size[A](t: Tree[A]): Int = fold(t)(const(1))((x, y) => x + y)

    def maximum(t: Tree[Int]): Int = fold(t)(identity)((x, y) => x max y)

    def depth[A](t: Tree[A]): Int = fold(t)(const(1))((x, y) => x max y)

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
      fold[A, Tree[B]](t)(f andThen Leaf.apply _)(Branch.apply _)
    }
  }

}

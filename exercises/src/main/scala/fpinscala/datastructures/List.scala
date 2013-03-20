package fpinscala.datastructures

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => ???
      case Cons(x, xs) => xs
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }

  def setHead[A](l: List[A])(h: A): List[A] =
    l match {
      case Nil => ???
      case Cons(x, xs) => Cons(h, xs)
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => ???
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0) { (_e, z) => z + 1 }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldLeft(l, Nil: List[B])((z, e) => Cons(f(e), z))

  def sum3(l: List[Int]): Int = foldLeft(l, 0) (_ + _)

  def product3(l: List[Int]): Int = foldLeft(l, 0) (_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0) ((acc, e) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) ((acc, e) => Cons(e, acc))

  def foldLeftViaRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((e, acc) => f(acc, e))

  def foldRightViaLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc, e) => f(e, acc))

  def appendViaFold[A](as: List[A], bs: List[A]) =
    foldRight(as, bs)((a, accB) => Cons(a, accB))

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(appendViaFold)

  def add1(l: List[Int]): List[Int] = map(l)(_ + 1)

  def double2Str(l: List[Double]): List[String] = map(l)( _.toString )

  def filter[A](l: List[A])(p: A => Boolean) =
    concat(map(l)(e => if (p(e)) List(e) else List()))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(p: A => Boolean) =
    flatMap(l)(e => if (p(e)) List(e) else List())

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(a + b, addPairwise(as, bs))
    }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: A => B => C): List[C] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a)(b), zipWith(as, bs)(f))
    }

  def hasSubsequence[A](as: List[A], bs: List[A]): Boolean = {
    def isPrefix(l: List[A], pre: List[A]) = {
      val zs = zipWith(l, pre)(a => b => a == b)
      foldRight(zs, true)(_ && _)
    }

    def hasSub(as: List[A], bs: List[A]): Boolean =
      as match {
        case Nil => false
        case Cons(x, xs) if isPrefix(as, bs) => true
        case Cons(x, xs) => hasSub(xs, bs)
      }

    hasSub(as, bs)
  }


}

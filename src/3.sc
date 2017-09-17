sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](a: A*): List[A] = {
    if (a.isEmpty) Nil else Cons(a.head, apply(a.tail: _*))
  }

  def tail[A](a: List[A]): List[A] = a match {
    case Nil => throw new Exception("Err Umm")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new Exception("Err Umm")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil if n>0 => throw new Exception("Err Umm")
    case Nil => Nil
    case Cons(_, t) if n>0 => drop(t, n-1)
    case Cons(h, t) => Cons(h, t)
  }


  // ??? Different
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if(f(h)) dropWhile(t, f) else Cons(h, dropWhile(t, f))
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Err Umm")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }


  // 3.7
  // No- Can only return to parent call. Has to go all the way up the call stack.

  // 3.8
  // Same

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0) {(_, b) => 1+b}
  }


  // 3.10

  // ???
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((_ + _))

  def product3(ns: List[Int]) = foldLeft(ns, 1)(_ * _)

}

val m = List(2,4)
//List.dropWhile(m, (a: Int) => a%2==0)
List.init(m)
List.length(m)
List.product3(m)
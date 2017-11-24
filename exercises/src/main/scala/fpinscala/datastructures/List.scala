package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =  l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case Cons(h, t) => Cons(h, t)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Cons(h, t) => Cons(h, t)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldedProduct(l: List[Double]): Double = {
    foldRight(l, 0.0)((x, y) => if (x == 0 || y == 0) 0 else x * y)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, y) => y + 1)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    List.foldLeft(l, List[A]())((acc, y) => Cons(y, acc))
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = List.foldRight(l, z)(f)

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = List.foldLeft(List.reverse(l), z)(f)

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = List.foldLeft(l, r)((acc, h) => Cons(h, acc))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil:List[A])(append)

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, addOne(t))
  }

  def addOneFolded(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doubleToString(t))
  }

  def doubleToStringFolded(l: List[Double]): List[String] =
    foldRight(l: List[Double], Nil:List[String])((h, t) => Cons(h.toString, t))

  // t is tail, h is head, why is it backwards I'm not sure, the signature is reversed in the book for foldLeft
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))
}

object testList {

  def main(args: Array[String]) {
    val list: List[Int] = List(1, 2, 3, 4, 5)
    val nilList: List[Nothing] = Nil

    println("tail")
    println(List.tail(list))
    println(List.tail(nilList))

    println("\nsetHead")
    println(List.setHead(list, 6))
    println(List.setHead(nilList, 1))

    println("\ndrop")
    println(List.drop(list, 3))
    println(List.drop(list, 1))
    println(List.drop(list, 0))
    println(List.drop(list, 5))
    println(List.drop(nilList, 3))

    println("\ndropWhile")
    println(List.dropWhile(list, (x: Int) => x < 3))
    println(List.dropWhile(list, (x: Int) => x < 7))
    println(List.dropWhile(list, (x: Int) => x < 4))

    println("\ninit")
    println(List.init(List(1, 2, 3)))
    println(List.init(List()))
    println(List.init(List(1, 2, 3, 4, 5, 6)))


    println("\nfoldedproduct stopping early")
    println(List.foldedProduct(List(1, 2, 3)))
    println(List.foldedProduct(List(0, 1, 2)))
    println(List.foldedProduct(List(1, 0, 2, 3)))
    // Nope, won't work. foldRight evaluates all values before calling the function f, so it will just return 0.

    println("\nlength of a list")
    println(List.length(List(1, 2, 3)))
    println(List.length(List()))
    println(List.length(List(1, 2, 3, 4)))
    println(List.length(List(1)))

    println("\nfoldleft")
    println("foldLeft sum: " + List.foldLeft(List(1, 2, 3, 4), 0)(_ + _))
    println("foldLeft product: " + List.foldLeft(List(1, 2, 3, 4), 1)(_ * _))
    println("foldLeft string concatenation: " + List.foldLeft(List("a", "b", "c", "d"), "")(_ + _))
    println("foldLeft length: " + List.foldLeft(List(1, 2, 3, 4), 0)((x, _) => x + 1))
    println("foldLeft length: " + List.foldLeft(List(1, 2, 3), 0)((x, _) => x + 1))
    println("foldLeft length: " + List.foldLeft(List(1), 0)((x, _) => x + 1))
    println("foldLeft length: " + List.foldLeft(List(), 0)((x, _) => x + 1))

    println("\nreverse")
    println("reverse: " + List.reverse(List(1, 2, 3, 4)))
    println("reverse: " + List.reverse(List()))
    println("reverse: " + List.reverse(List("a", "b", "c")))
    println("reverse: " + List.reverse(List(1)))

    println("\nappend with foldLeft")
    println(List.appendViaFoldLeft(List(1, 2, 3), List(4, 5, 6)))
    println(List.appendViaFoldLeft(List("a", "b"), List("b", "c")))
    println(List.appendViaFoldLeft(List(), List("a")))
    println(List.appendViaFoldLeft(List(), List()))

    println("\nconcat")
    println(List.concat(List(List(1, 2, 3), List(4, 5))))

    println("\naddone")
    println(List.addOne(List(1, 2, 3, 4)))
    println(List.addOne(List()))
    println(List.addOne(List(1)))

    println("\ndoubletostring")
    println(List.doubleToString(List(1, 2, 3, 4)))
    println(List.doubleToString(List()))
    println(List.doubleToString(List(1)))

    println("\naddonefolded")
    println(List.addOneFolded(List(1, 2, 3, 4)))
    println(List.addOneFolded(List()))
    println(List.addOneFolded(List(1)))

    println("\ndoubletostringfolded")
    println(List.doubleToStringFolded(List(1, 2, 3, 4)))
    println(List.doubleToStringFolded(List()))
    println(List.doubleToStringFolded(List(1)))

    println("\nmap")
    println(List.map(List(1, 2, 3, 4))(x => x * 2))
    println(List.map(List("1", "2", "3", "4"))(x => x + "a"))
  }
}
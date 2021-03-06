package fpinscala.gettingstarted

// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // Another implementation of `factorial`, this time with a `while` loop
  def factorial2(n: Int): Int = {
    var acc = 1
    var i = n
    while (i > 0) { acc *= i; i -= 1 }
    acc
  }

  // Exercise 1: Write a function to compute the nth fibonacci number

  def fib(n: Int): Int = {

    def go(counter: Int, x: Int, y: Int): Int = {

      if (counter == n) x
      else go(counter + 1, y, y + x)
    }

    go(0, 0, 1)
  }

  // This definition and `formatAbs` are very similar..
  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // We can generalize `formatAbs` and `formatFactorial` to
  // accept a _function_ as a parameter
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }
}

object FormatAbsAndFactorial {

  import MyModule._

  // Now we can use our general `formatResult` function
  // with both `abs` and `factorial`
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}

object TestFib {

  import MyModule._

  // test implementation of `fib`
  def main(args: Array[String]): Unit = {
    println("Expected: 0, 1, 1, 2, 3, 5, 8")
    println("Actual:   %d, %d, %d, %d, %d, %d, %d".format(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))
  }
}

// Functions get passed around so often in FP that it's
// convenient to have syntax for constructing a function
// *without* having to give it a name
object AnonymousFunctions {
  import MyModule._

  // Some examples of anonymous functions:
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 1))
    println(formatResult("increment3", 7, x => x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x => { val r = x + 1; r }))
  }
}

object MonomorphicBinarySearch {

  // First, a binary search implementation, specialized to `Double`,
  // another primitive type in Scala, representing 64-bit floating
  // point numbers
  // Ideally, we could generalize this to work for any `Array` type,
  // so long as we have some way of comparing elements of the `Array`
  def binarySearch(ds: Array[Double], key: Double): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val d = ds(mid2) // We index into an array using the same
                         // syntax as function application
        if (d == key) mid2
        else if (d > key) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, ds.length - 1)
  }

}

object PolymorphicFunctions {

  // Here's a polymorphic version of `binarySearch`, parameterized on
  // a function for testing whether an `A` is greater than another `A`.
  def binarySearch[A](as: Array[A], key: A, gt: (A,A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 4: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 5: Implement `compose`

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}

object testPolymorphicFunctions {

  import PolymorphicFunctions._

  def main(args: Array[String]): Unit = {

    testIsSorted()
    testCurrying()
    testUncurrying()
    testCompose()
  }

  def formatIsSorted[A](res: Boolean, array: Array[A]): String = {

    if (res) s"Array ${array.mkString(",")} is sorted"
    else s"Array ${array.mkString(",")} is not sorted"
  }

  def testIsSorted(): Unit = {

    val arr1: Array[Int] = Array(1, 2, 3, 4, 5, 6)
    val arr2: Array[Int] = Array(6, 5, 4, 3, 2, 1)
    val arr3: Array[Int] = Array(1, 3, 2, 6, 4, 5)
    val arr4: Array[String] = Array("a", "b", "c", "d")
    val arr5: Array[String] = Array("b", "c", "a", "d")
    val arr6: Array[Double] = Array(1.2, 1.3, 2.1)
    val arr7: Array[Double] = Array(1.3, 2.1, 1.4)
    val arr8: Array[Int] = Array(1)
    val arr9: Array[Int] = Array()

    println(formatIsSorted(isSorted(arr1, (x: Int, y: Int) => x > y), arr1))
    println(formatIsSorted(isSorted(arr2, (x: Int, y: Int) => x > y), arr2))
    println(formatIsSorted(isSorted(arr3, (x: Int, y: Int) => x > y), arr3))
    println(formatIsSorted(isSorted(arr4, (x: String, y: String) => x > y), arr4))
    println(formatIsSorted(isSorted(arr5, (x: String, y: String) => x > y), arr5))
    println(formatIsSorted(isSorted(arr6, (x: Double, y: Double) => x > y), arr6))
    println(formatIsSorted(isSorted(arr7, (x: Double, y: Double) => x > y), arr7))
    println(formatIsSorted(isSorted(arr8, (x: Int, y: Int) => x > y), arr8))
    println(formatIsSorted(isSorted(arr9, (x: Int, y: Int) => x > y), arr9))
  }

  def testCurrying(): Unit = {

    val a: Int => Int => Int = curry((x: Int, y: Int) => x * y)
    val a1: Int => Int = a(2)
    val a2: Int = a1(3)
    println(a2)

    val b: String => Int => String = curry((x: String, y: Int) => x + y.toString)
    val b1: Int => String = b("hello")
    val b2: String = b1(1)
    println(b2)

    val c: String => Int => (String, Int) = curry((x: String, y: Int) => (x, y))
    val c1: Int => (String, Int) = c("hello")
    val c2: (String, Int) = c1(1)
    println(c2)
  }

  def testUncurrying(): Unit = {

    val a = uncurry(curry((x: Int, y: Int) => x * y))
    println(a(2, 3))

    val b = uncurry(curry((x: String, y: Int) => x + y.toString))
    println(b("hello ", 1))

    val c = uncurry(curry((x: String, y: Int) => (x, y)))
    println(c("hello", 2))
  }

  def testCompose(): Unit = {

    val a: Int => Int = compose((x: Int) => x * 2 , (y: Int) => y - 2)
    println(a(10))

    val a1: Int => String = (x: Int) => x.toString
    val b1: String => Double = (x: String) => x.toDouble
    println(b1(a1(10)))
  }
}
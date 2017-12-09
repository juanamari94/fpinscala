package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(a) => l(a)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def foldedSize[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def foldedMaximum(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def foldedDepth[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _ max _)

  def foldedMap[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}

object testTree {

  def main(args: Array[String]): Unit = {

    println("\nsize")
    println(Tree.size(Leaf(1)))
    println(Tree.size(Branch(Leaf(1), Leaf(2))))
    println(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))

    println("\nmax")
    println(Tree.maximum(Leaf(1)))
    println(Tree.maximum(Branch(Leaf(1), Leaf(2))))
    println(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    println(Tree.maximum(Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(1), Leaf(2)))))

    println("\ndepth")
    println(Tree.depth(Leaf(1)))
    println(Tree.depth(Branch(Leaf(1), Leaf(2))))
    println(Tree.depth(Branch(Branch(Leaf(3), Leaf(4)), Leaf(2))))

    println("\nmap")
    println(Tree.map(Branch(Leaf(1), Leaf(2)))(value => value + 1))

    println("\nfoldedSize")
    println(Tree.foldedSize(Leaf(1)))
    println(Tree.foldedSize(Branch(Leaf(1), Leaf(2))))
    println(Tree.foldedSize(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))

    println("\nfoldedMaximum")
    println(Tree.foldedMaximum(Leaf(1)))
    println(Tree.foldedMaximum(Branch(Leaf(1), Leaf(2))))
    println(Tree.foldedMaximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    println(Tree.foldedMaximum(Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(1), Leaf(2)))))

    println("\nfoldedDepth")
    println(Tree.foldedDepth(Leaf(1)))
    println(Tree.foldedDepth(Branch(Leaf(1), Leaf(2))))
    println(Tree.foldedDepth(Branch(Branch(Leaf(3), Leaf(4)), Leaf(2))))

    println("\nfoldedMap")
    println(Tree.foldedMap(Branch(Leaf(1), Leaf(2)))(value => value + 1))

  }
}
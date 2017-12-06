package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => max(left).max(max(right))
  }
}

object testTree {

  def main(args: Array[String]): Unit = {

    println("\nsize")
    println(Tree.size(Leaf(1)))
    println(Tree.size(Branch(Leaf(1), Leaf(2))))
    println(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))

    println("\nmax")
    println(Tree.max(Leaf(1)))
    println(Tree.max(Branch(Leaf(1), Leaf(2))))
    println(Tree.max(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    println(Tree.max(Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(1), Leaf(2)))))
  }
}
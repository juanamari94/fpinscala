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
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }
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
  }
}
package com.qtamaki.lh

sealed abstract class Heap[T]
case class Empty[T: Ordering]() extends Heap[T]
case class Tree[T: Ordering](rank: Int, node: T, left: Heap[T], right: Heap[T]) extends Heap[T]

object Heap {
  private def rank[T](h: Heap[T]): Int = h match {
    case Empty() => 0
    case Tree(x, _, _, _) => x
    case _ => throw new Exception()
  }
  private def makeTree[T: Ordering](node: T, h1: Heap[T], h2: Heap[T]): Heap[T] = if (rank(h1) >= rank(h2)) {
    Tree(rank(h2) + 1, node, h1, h2)
  } else {
    Tree(rank(h1) + 1, node, h2, h1)
  }
  def merge[T](heap1: Heap[T], heap2: Heap[T])(implicit ord: Ordering[T]): Heap[T] = (heap1, heap2) match {
    case (Empty(), h) => h
    case (h, Empty()) => h
    // case (h1 @ Tree(_, x, a1, b1), h2 @ Tree(_, y, a2, b2)) if x < y => makeTree(x, a1, merge(b1, h2))
    // case (h1 @ Tree(_, x, a1, b1), h2 @ Tree(_, y, a2, b2)) => makeTree(x, a2, merge(b2, h1))
    case (h1@Tree(_, node1, left1, right1), h2@Tree(_, node2, left2, right2)) => {
      import ord._
      if (node1 < node2) {
        makeTree(node1, left1, merge(right1, h2))
      } else {
        makeTree(node2, left2, merge(right2, h1))
      }
    }
    case _ => throw new Exception()
  }
  def insert[T: Ordering](node: T, heap: Heap[T]): Heap[T] = {
    merge(Tree(1, node, Empty(), Empty()), heap)
  }
  def findMin[T: Ordering](h: Heap[T]): Option[T] = h match {
    case Tree(_, node, _, _) => Some(node)
    case Empty() => None
  }
  def deleteMin[T: Ordering](h: Heap[T]): Heap[T] = h match {
    case Tree(_, x, a, b) => merge(a, b)
    case e => e
  }
  def listToHeap[T: Ordering](xs: List[T]) = xs.foldRight[Heap[T]](Empty())(insert _)
  def heapToList[T: Ordering](h: Heap[T]): List[T] = h match {
    case Empty() => List()
    //case t:Tree[T] => f(implicit c:ClassManifest[T])indMin(t) :: heapToList(deleteMin(t))
    case t: Tree[_] => {
      val n: List[T] = findMin(t).toList
      val list: List[T] = heapToList(deleteMin(t))
      n ::: list
    }
  }
}

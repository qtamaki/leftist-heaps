package com.qtamaki.heaps.binomial

case class Tree[+T](rank: Int, elem: T, trees: List[Tree[T]])

object Tree {
	def link[T](t1: Tree[T], t2: Tree[T])(implicit ord: Ordering[T]) = if (ord.lteq(t1.elem, t2.elem)){ // =<
	  Tree(t1.rank + 1, t1.elem, t2 :: t1.trees)
	}else{
	  Tree(t1.rank + 1, t2.elem, t1 :: t2.trees)
	}
	
	def insTree[T: Ordering](tree: Tree[T], treeList: List[Tree[T]]): List[Tree[T]] = treeList match {
	  case Nil => tree :: Nil
	  case x::xs => if (tree.rank < x.rank) {
	    tree :: x :: xs
	  }else{
	    assert(tree.rank == x.rank) // same rank
	    insTree(link(tree, x), xs)
	  }
	}
	
	def insert[T: Ordering](elem: T, trees: List[Tree[T]]): List[Tree[T]] = insTree(Tree(0, elem, Nil), trees)
	
	def makeTreeFromSeq[T: Ordering](src: Seq[T]) = src.foldLeft(List.empty[Tree[T]])((ts, x) => insert(x, ts))
	
	def merge[T: Ordering](ts1: List[Tree[T]], ts2: List[Tree[T]]): List[Tree[T]] = (ts1, ts2) match {
	  case (xs, Nil) => xs
	  case (Nil, ys) => ys
	  case (x::xs, y::ys) => if(x.rank < y.rank) {
	    x :: merge(xs, y::ys)
	  }else if (y.rank < x.rank){
	    y :: merge(x::xs, ys)
	  }else{
	    insTree(link(x,y), merge(xs,ys))
	  }
	}
	
	def removeMinTree[T](trees: List[Tree[T]])(implicit ord: Ordering[T]): (Tree[T], List[Tree[T]]) = trees match {
	  case x :: Nil => (x, Nil)
	  case x :: xs => {
		  val (xx, xxs) = removeMinTree(xs)
		  if(ord.lteq(x.elem,xx.elem)){
		    (x, xs)
		  }else{
		    (xx, x :: xxs)
		  }
	  }
	  case Nil => throw new RuntimeException("Error!")
	}
	
	def findMin[T: Ordering](trees: List[Tree[T]]):T = {
	  val (x, _) = removeMinTree(trees)
	  x.elem
	}
	
	def deleteMin[T: Ordering](trees: List[Tree[T]]): List[Tree[T]] = removeMinTree(trees) match {
	  case (Tree(_, x, ts1), ts2) => merge(ts1.reverse, ts2) // reverse
	  case _ => throw new RuntimeException("Error!")
	}
	
	def publishTree[T: Ordering](heap: List[Tree[T]]): String = {
	  (for(x <- heap)yield{
	    "rank:" + x.rank + ", elem:" + x.elem + ", trees: (" + publishTree(x.trees) + ")"
	  }).mkString(", ")
	}
	
	def pretty[T: Ordering](heap: List[Tree[T]], deps: Int): String = {
	  (for(x <- heap)yield{
	    ("\t" * deps) + x.elem + "(" + x.rank + ")\n" +
	    pretty(x.trees, deps + 1)
	  }).mkString("\n")
	}
}
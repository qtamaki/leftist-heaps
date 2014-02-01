package com.qtamaki.heaps.binomial

  import org.specs2.mutable._
  import org.specs2.specification.Scope

  class HelloWorldSpec extends Specification {

    "The Binomial Heap" should {
      "contain 11 characters" in new Trees {
        heap must have size (3)
      }
      "link 1 " in {
        val x = Tree.link(Tree(0, "a", Nil), Tree(0, "b", Nil))
        x.rank === 1
        x.elem === "a"
        x.trees must have size (1)
        val y = x.trees.head
        y.rank === 0
        y.elem === "b"
        y.trees must be empty
      }
      "link 2 " in {
        val t1 = Tree.link(Tree(0, "a", Nil), Tree(0, "b", Nil))
        tree2Assert(t1, "a", "b")
        val t2 = Tree.link(Tree(0, "c", Nil), Tree(0, "d", Nil))
        tree2Assert(t2, "c", "d")
        lteq("a","b") must beTrue
        lteq("a","c") must beTrue
        t1.rank === t2.rank
        val t3 = Tree.link(t1,t2)
        treeAssert(t3, 2, "a", t2 :: t1.trees)
      }
      "intTree 1" in {
        val ts1 = Tree.insTree(Tree(0, "a", Nil), Nil)
        ts1 must have size (1)
      }
      "intTree 2" in {
        val ts1 = Tree.insTree(Tree(0, "a", Nil), Nil)
        val ts2 = Tree.insTree(Tree(0, "b", Nil), ts1)
        val ts3 = Tree.insTree(Tree(0, "c", Nil), ts2)
        val ts4 = Tree.insTree(Tree(0, "d", Nil), ts3)
        val ts5 = Tree.insTree(Tree(0, "e", Nil), ts4)
        ts3 must have size (2)
        treeAssert(ts3.head, 0, "c", Nil)
        treeAssert(ts3.tail.head, 1, "a", Tree(0, "b", Nil)::Nil)
        ts4 must have size (1)
        treeAssert(ts4.head, 2, "a", Tree(1, "c", Tree(0, "d", Nil) :: Nil) :: Tree(0, "b", Nil) :: Nil)
        ts5 must have size (2)
        treeAssert(ts5.tail.head, 2, "a", Tree(1, "c", Tree(0, "d", Nil) :: Nil) :: Tree(0, "b", Nil) :: Nil)
        treeAssert(ts5.head, 0, "e", Nil)
      }
    }
    
    def tree2Assert[T](t: Tree[T], e1: T, e2: T) = {
        val t2 = t.trees.head
        treeAssert(t, 1, e1, t2::Nil)
        treeAssert(t2, 0, e2, Nil)
      
    }
    def treeAssert[T](t: Tree[T], r: Int, e: T, ts: List[Tree[T]]) = {
      t.rank === r
      t.elem === e
      t.trees === ts
    }
    
    def lteq(x:String, y:String)(implicit ord: Ordering[String]):Boolean = {
      ord.lteq(x,y)
    }
  }

trait Trees extends Scope {
    val heap:List[Tree[String]] = Tree.makeTreeFromSeq(List("a","b","c","d","e","f","g","h","i","j","k"))
} 
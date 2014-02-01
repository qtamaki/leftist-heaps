import com.qtamaki.lh.Heap
import com.qtamaki.lh.Empty
import com.qtamaki.heaps.binomial.Tree

object Main {

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8)
    val h1 = Heap.merge(Empty, Empty)(Ordering[Int])
    val h2 = Heap.insert(1, Empty)
    val h3 = Heap.listToHeap(list)
    println("hello")
    val heap:List[Tree[String]] = Tree.makeTreeFromSeq(List("a","b","c","d","e","f","g","h","i","j","k","l","m","n"))
    println(heap)
    print(Tree.pretty(heap, 0))
  }
}
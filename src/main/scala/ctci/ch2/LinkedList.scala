package ctci.ch2


object LinkedList {
  def removeDuplicates[T](l: List[T]): List[T] =
    l.foldLeft(List[T]())((acc, curr) ⇒
      if(! acc.exists(_ == curr) ) curr :: acc; else acc)
  def ssize[T](l: List[T]) : Int = {
    def helper(ts: List[T], s: Int) : Int = {
      ts match  {
        case (h :: t) ⇒ helper(t, s+1)
        case _ ⇒ s
      }
    }
    helper(l, 0)
  }
  def kth[T](ts: List[T], k: Int): Option[T] = {

    def helper(ts: List[T], s: Int) : Option[T] = {
      ts match  {
        case (h :: t) if s > 0 ⇒ helper(t, s-1)
        case (h :: t) if s == 0 ⇒ Some(h)
        case _ ⇒ None
      }
    }
    helper(ts, k)
  }
  def kth[T](it:Iterator[T], k: Int): Option[Iterator[T]] = {
    for(i ← 0 to k-1)
      if(it.hasNext)
        it.next
      else return None
    Some(it)
  }
  def kthLast[T](input:Iterator[T], k: Int): Option[T] = {
    def helper(curr: Iterator[T], front: Option[Iterator[T]]): Option[T] = {
      front match {
        case Some(i) if i.hasNext ⇒
          curr.next
          i.next
          helper(curr, Some(i))
        case Some(i) ⇒  Some(curr.next)
        case _ ⇒ None
      }
    }
    val (it, itFront) = input.duplicate
    helper(it,  kth[T](itFront, k) )
  }

  def middle[T](input:Iterator[T]): T = {
    def helper(it: Iterator[T], fastIt: Iterator[T]): T = {

      if(fastIt.hasNext)
        fastIt.next
      else
        return it.next
      if(fastIt.hasNext) {
        fastIt.next
        it.next
      }
      helper(it, fastIt)
    }
    val (it, itFast) = input.duplicate
    helper(it,  itFast)
  }

  def partition(input: List[Int], p: Int) : List[Int] = 
    input.filter(_ > p) ::: input.filter( _ <= p )
}

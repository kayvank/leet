package ctci.ch2

import scala.collection.mutable._

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

  def sumList(l1: List[Int], l2: List[Int]): List[Int] = {
    def helper(pairs: (List[Int], List[Int]),
                carry: Int, res: List[Int]): List[Int] =
      pairs match {
        case ( (h1 :: t1), (h2 :: t2) ) ⇒
          helper((t1, t2),(h1+h2+carry)/10, res ::: List((h1+h2+carry) % 10))
        case ((h :: t), _)  if (h + carry)>9 ⇒
          helper( (t, Nil), (h+carry)/10, res ::: List((h+carry) % 10))
        case ((h :: t), _) ⇒ res ::: h+carry :: t
        case (_, (h :: t))  if (h + carry)>9 ⇒
          helper( (Nil, t), (h+carry)/10, res ::: List((h+carry) % 10))
        case (_, (h :: t)) ⇒ res ::: h+carry :: t
        case _ if carry > 0 ⇒ res ::: carry :: Nil
      }
    helper((l1,l2), 0, Nil) 
  }

  def isPalindrome1[T](ls: List[T]): Boolean =
    ls.foldLeft(List[T]()) ( (acc, curr) ⇒ curr :: acc ) == ls

  def isPalindrome2[T](ls: List[T]): Boolean = ls.reverse == ls

  def firstHalfReverse[T](ls: List[T]): List[T] = {
    def helper(itPair:(Iterator[T], Iterator[T]),
               store: List[T]): (List[T], Iterator[T]) =  {
      if(itPair._2.hasNext)
        itPair._2.next
      else
        return  (store, itPair._1)
      if(itPair._2.hasNext) {
        itPair._2.next
        helper(itPair, itPair._1.next :: store )
      }
      else return (store,itPair._1)
    }
    helper(ls.toIterator.duplicate, List[T]())._1
  }
  def isPalindrome3[T](ls: List[T]): Boolean = {
   true 
}
}

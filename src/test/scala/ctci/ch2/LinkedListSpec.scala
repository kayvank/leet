package ctci.ch2

import org.specs2._

class LinkedListSpec extends Specification { def is = s2"""
Cracking the code interview, chapter 2 linkeList
  remove duiplicates $e1
  return the size to last $e2
  kth node $e3
  kth iterator $e4
  kthLast iterator $e5
  find middle element event $e6  odd $e7
  partition list $e8
"""
  import LinkedList._

  def e1 =  removeDuplicates[Int](List(1,2,3,4,1,3,4,4)).size must_== 4
  def e2 = ssize[Int](List(1,2,3,4)) must_== 4
  def e3 = kth[Int](List(1,2,3,4,5), 2).get must_== 3 
  def e4 = kth[Int](List(1,2,3,4,5).toIterator, 2).get.next  must_== 3
  def e5 = kthLast[Int](List(1,2,3,4,5).toIterator, 2).get  must_== 4
  def e6 = middle[Int](List(1,2,3,4,5, 6, 7).toIterator)  must_== 4
  def e7 = middle[Int](List(1,2,3,4,5, 6, 7, 8).toIterator)  must_== 5
  def e8 = partition(List(11,20,9,7,15,4,9,6,5,4), 9)  must_== List(11,20,15,9,7,4,9,6,5,4)
}

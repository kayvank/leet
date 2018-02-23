package intrvw

import org.specs2._

class LeetSpec extends Specification { def is = s2"""

Leet interview prep qestions
  find a,b indices such that a+b=target $e1
  optimized find a,b indices such that a+b=target $e2
  Input: (2 -> 4 -> 3) + (5 -> 6 -> 4 → 3)  Output: 7 -> 0 -> 8 -_ 3
  Explanation: 342 + 3465 = 3807
"""
  import Leet._
    
  def e1 =  {
    val input = Vector(2, 7, 11, 15)
    val actual = (0,1)
    actual must_== sumFinder(input, 9).get
  }
  def e2 =  {
    val input = Vector(7, 11, 15, 2)
    val computed = sumFinderOptim(input, 9).get
    (input(computed._1) + input(computed._2) ) must_== 9
  }
  def e3 =  {
    val num1 = List[Int](2,4,3)
    val num2 = List[Int](5,6,4)
    val actual = List[Int](7, 0 , 8)
    1 must_== 1
  }

}


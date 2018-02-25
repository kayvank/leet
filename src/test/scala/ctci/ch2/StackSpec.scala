package ctci.ch2

import org.specs2._

class StackSpec extends Specification { def is = s2"""
Stack impl
  push $e1
  stack min $e2
"""
  def e1 = {
    var myStack = _Stack.apply[Int]()
    myStack = _Stack.push[Int](myStack)(10)
    myStack = _Stack.push[Int](myStack)(20)
    myStack = _Stack.push[Int](myStack)(30)
    val c1 = _Stack.pop(myStack)
    val c2 = _Stack.pop(c1.get._1)
    val c3 = _Stack.pop(c2.get._1)
    c1.get._2 must_== (30)
    c2.get._2 must_== (20)
    c3.get._2 must_== (10)
  }
  def e2 = {
    val myStack = StackMin.apply()
    val s1 = StackMin.push(myStack)(10)
    val s2 = StackMin.push(s1)(5)
    val s3 = StackMin.push(s2)(2)
    println(s"s3 = (2,5,10),2,2 ${StackMin.min(s3)}")
    val (s4,poped_2) = StackMin.pop(s3).get
    val min_4 = StackMin.min(s4) //5
    val min_3 = StackMin.min(s3) //2
    val s5 = StackMin.push(s4)(1)

    println(s"s4 = : ${StackMin.min(s4)}")
    val s6 = StackMin.push(s5)(-1)
    println(s"s5 = in should be: ${StackMin.min(s5)}")

    
    println(s" ***** min_3 = ${min_3}")
    println(s" ***** min_4 = ${min_4}")
    println(s" ***** s1 = ${s1}")
    println(s" ***** s2 = ${s2}")
    println(s" ***** s3 = ${s3}")
    println(s" ***** s4 = ${s4}")
    println(s" ***** s5 = ${s5}")
    println(s" ***** s6 = ${s6}")
    min_3 must_== 2
    min_4 must_== 5
  }
}

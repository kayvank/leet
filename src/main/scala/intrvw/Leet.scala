package intrvw

import scala.collection.immutable._

object Leet {
  def sumFinder(v: Vector[Int], target: Int): Option[(Int, Int)] =  {
    for(i ← 0 to v.size-1) {
      for (j ← i to v.size-1) {
        if(v(i) + v(j) == target)
          return Some((i,j))
      }
    }
    return None
  }
  def sumFinderOptim(v: Vector[Int], target: Int): Option[(Int, Int)] =  { 
    var m: Map[Int, Int] =  new HashMap[Int, Int]()
    for(i ← 0 to v.size-1) {
      val diff=target - v(i)
      if(m.get(diff).isDefined)
        return Some(i, m(diff))
      else m+=(v(i) → i)
    }
    None
  }
}

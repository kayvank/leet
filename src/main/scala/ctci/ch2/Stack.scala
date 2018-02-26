package ctci.ch2

case class _Stack[T](store: List[T]=Nil) 

object _Stack {
  def apply[T](): _Stack[T] = _Stack[T](List[T]())

  def push[T]: _Stack[T] ⇒  T ⇒ _Stack[T] = {
    println(s"pushing to stack")
    stack ⇒ t ⇒ _Stack(t :: stack.store)
}
  def pop[T]: _Stack[T] ⇒ Option[(_Stack[T], T)] = stack ⇒ {
    val ret = stack.store.headOption
    if(ret.isDefined)
      Some(_Stack(stack.store.tail), ret.get)
    else None
  }
}

/**
 * design stack that has a min function
 **/
case class StackMin(
  store: List[(Int, Int)],  // use a case class instead of pair for clarity
  min: Int)

object StackMin {
  def apply[Int](): StackMin = StackMin(Nil, Int.MaxValue)

  def push: StackMin ⇒ Int ⇒ StackMin = stack ⇒ i ⇒  {
    val _min = if(stack.min >= i ) i; else stack.min
    StackMin(min=_min, store=(i, _min) :: stack.store)
  }

  def pop: StackMin ⇒ Option[(StackMin, Int)] =  stack ⇒ {
    val ret = stack.store.headOption
    if (ret.isDefined)
      Some(
        StackMin(store=stack.store.tail, min=stack.store.tail.head._2), ret.get._1
      )
   else None 
  }
  def min: StackMin ⇒ Int = stack ⇒ stack.min
}

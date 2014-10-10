class Streams {

  def map[T,Q](l: List[T], f: T=>Q): List[Q] = {
    for ( i <- l) yield f(i)
  }

  def filter[T](l: List[t], f: T=>Boolean): List[T] = {
    for ( i <- l; if(f(i)) ) yield i
  }

  def flatmap[T,Q](l: List[T], f: T=>List[Q]): List[Q] = {
    for ( i <- l; j <- f(i)) yield j
  }

}


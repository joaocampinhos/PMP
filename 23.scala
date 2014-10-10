class Streams {

  def countNeg (l: List[Int]): Int = {
    (for( i <- l ; if (i<0)) yield i).length
  }

  def repeat(n: Int) = {
    for ( i <- List.range(1, n+1); j <- List.range(1, i+1) ) yield i
  }

}

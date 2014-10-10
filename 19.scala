
class Streams {

  def next(l: List[Int]): List[Int] = {
    l match {
      case List(x) => List(x)
      case x::y::xs => (x+y)::next(y::xs)
    }
  }

  def dpascal_aux(l: List[Int]): Stream[List[Int]] =
    l #:: dpascal_aux(1::next(l))

  val dpascal: Stream[List[Int]] = dpascal_aux(List(1))

  val cpascal: Stream[List[Int]] = List(1) #:: (cpascal map (l => 1::next(l)))
}

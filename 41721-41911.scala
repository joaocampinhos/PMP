/*
 * João Campinhos 41721
 * Pedro Durães   41911
 */

object Decimal {

  type Real = Stream[Int]

  //A
  def fromDouble(d: Double): Real =
    Stream.cons((d*10).toInt, fromDouble((d*10-(d*10).toInt)))

  //B
  def fromFraction(num: Int, den: Int): Real =
    Stream.cons(num*10/den, fromFraction((num*10%den), den))

  //C
  def sum(a:Real, b:Real) = (a zip b) map {
    case (x,y) =>
      if (x+y >= 10) {
        (x+y)-10
      }
      else x+y
  }

  //D
  val MAXPER = 1000
  def per(r: Real): Int = peraux(r.tail, Stream.cons(r.head, Stream.empty))
  def peraux(r: Real, aux: Real): Int = {
    if(aux.length > MAXPER) 0
    else
      if(cmp(r.take(aux.length),aux) == true) 1
      else peraux(r.tail, aux.append(Stream.cons(r.head, Stream.empty)))
  }

  def cmp(a: Real, b: Real): Boolean =
    (a, b) match {
      case (x #:: xs, y #:: ys) =>
        if (x == y) cmp(xs,ys)
        else false
      case _ => true
    }

  //E
  def toDouble(r: Real): Double =
    if (r.isEmpty) 0 else r.head.toDouble/10+toDouble(r.tail.take(15))/10

  //F
  val ints: Real = Stream.from(1)

}

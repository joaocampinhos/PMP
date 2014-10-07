object Decimal {

  type Real = Stream[Int]

  val zero: Real = Stream.cons(0, zero)
  val quarter: Real = Stream.cons(2, Stream.cons(5, zero))
  val third: Real = Stream.cons(3, third)
  val one: Real = Stream.cons(9, one)

  def fromDouble(d: Double): Real =
    Stream.cons((d*10).toInt, fromDouble((d*10-(d*10).toInt)))

  def fromFraction(num: Int, den: Int): Real =
    Stream.cons(num*10/den, fromFraction((num*10%den), den))

  //def sum(a: Real, b: Real): Real

  //val MAXPER = 1000
  //def per(r: Real): Int

  def toDouble(r: Real): Double =
    if (r.isEmpty) 0 else r.head.toDouble/10+toDouble(r.tail.take(15))/10

  val ints: Real = Stream.from(1)

  //-------------------------------------------------------
  //Só para testar
  def main(args: Array[String]) = {
    //a
    //val x = fromDouble(0.1234568)

    //b
    //val x = fromFraction(1,7)

    //c
    //

    //d
    //

    //e
    val a: Real = Stream.cons(1, Stream.cons(5, Stream.cons(3,zero)))
    val x = toDouble(third)

    //f
    //ints take 10 print

    //x take 10 print
    println(x)

  }
}

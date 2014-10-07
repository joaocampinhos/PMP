object Decimal {

  type Real = Stream[Int]

  val zero: Real = Stream.cons(0, zero)
  val quarter: Real = Stream.cons(2, Stream.cons(5, zero))
  val third: Real = Stream.cons(3, third)
  val one: Real = Stream.cons(9, one)

  //def fromDouble(d: Double): Real

  //def fromFraction(num: Int, den: Int): Real

  //def sum(a: Real, b: Real): Real

  //val MAXPER = 1000
  //def per(r: Real): Int

  //def toDouble(r: Real): Double

  //val ints: Real

  //-------------------------------------------------------
  //Só para testar
  def main(args: Array[String]) = {
    third take 5 print
  }
}

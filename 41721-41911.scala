object Decimal {

  type Real = Stream[Int]

  val zero: Real = Stream.cons(0, zero)
  val quarter: Real = Stream.cons(2, Stream.cons(5, zero))
  val third: Real = Stream.cons(3, third)
  val one: Real = Stream.cons(9, one)

  // Não sei se tou a gerar bem porque o noise é estranho.
  // TODO: remover primeiro digito
  def fromDouble(d: Double): Real = Stream.cons(d.toInt, fromDouble((d-d.toInt)*10))

  //def fromFraction(num: Int, den: Int): Real

  //def sum(a: Real, b: Real): Real

  //val MAXPER = 1000
  //def per(r: Real): Int

  //def toDouble(r: Real): Double

  //val ints: Real

  //-------------------------------------------------------
  //Só para testar
  def main(args: Array[String]) = {
    val x = fromDouble(0.1234)
    x take 10 print
  }
}

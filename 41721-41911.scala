object Decimal {

  type Real = Stream[Int]

  val zero: Real = Stream.cons(0, zero)
  val quarter: Real = Stream.cons(2, Stream.cons(5, zero))
  val third: Real = Stream.cons(3, third)
  val one: Real = Stream.cons(9, one)

  // Não sei se tou a gerar bem porque o noise é estranho.
  // TODO: remover primeiro digito
  def fromDouble(d: Double): Real = Stream.cons(d.toInt, fromDouble((d-d.toInt)*10))

  // TODO: remover primeiro digito
  def fromFraction(num: Int, den: Int): Real = Stream.cons(num/den, fromFraction((num%den)*10, den))

  //def sum(a: Real, b: Real): Real

  //val MAXPER = 1000
  //def per(r: Real): Int

  def toDouble(r: Real): Double = r.head.toDouble/10+toDouble(r.tail)

  val ints: Real = Stream.from(1)

  //-------------------------------------------------------
  //Só para testar
  def main(args: Array[String]) = {
    val x = toDouble(Stream.cons(1,Stream.cons(3,Stream.empty)))
    //println(1/3)
    //println(((1%3)*10)/3)
    //x take 10 print
    println(x);
  }
}

abstract class ZList[+T]
case object ZNil extends ZList[Nothing]
case class ZNode[T](head: T, tail: Unit=>ZList[T]) extends ZList[T]

object Aula2 {

  def main(args: Array[String]) = {
    //val kkk = ztake(zints, 1)
    //println(ztake(zints, 1))
    //println(zdrop(zints, 1))
    println(zshow(zzeros_ones))
    println(zshow(zzeros_ones_prof))
    println(zshow(zfib))
    println(zindex(zfib, 200))
  }

  val zints: ZList[Int] = ZNode(1, u=>zadd(zints,1))
  
  def zhd[T](l: ZList[T]) = {
    l match {
      case ZNil => error("zhd: head of empty zlist")
      case ZNode(x,_) => x
    }
  }  

  def ztl[T](l: ZList[T]) =
  {
    l match {
      case ZNil => error("ztl: tail of empty zlist")
      case ZNode(x,xs) => xs()
    }
  }

  def zlen[T](l: ZList[T]): Int =
  {
    l match {
      case ZNil => 0
      case ZNode(x,xs) => 1 + zlen(xs())
    }
  }

  def zadd(l: ZList[Int], n: Int): ZList[Int] =
  {
    l match {
      case ZNil => ZNil
      case ZNode(x,xs) => ZNode(x+n, u=>zadd(xs(), n))
    }
  }

  def zshowX[T](l: ZList[T], n: Int): List[T] =
  {
    if (n == 0) Nil
    else l match {
      case ZNil => Nil
      case ZNode(x,xs) => x::zshowX(xs(), n-1)
    }
  }

  def zshow[T](l: ZList[T]) = zshowX(l, 10)

  def ztake[T](l: ZList[T], n: Int): ZList[T] = {
    if ( n == 0) l
    else 
      l match {
        case ZNil => sys.error("ztake: error")
        case ZNode(x,xs) => ZNode(x, u=>ztake(xs(), n-1))
      }
  }

  def zdrop[T](l: ZList[T], n: Int): ZList[T] = {
    if ( n == 0) l
    else 
      l match {
        case ZNil => sys.error("zdrop: error")
        case ZNode(x,xs) => zdrop(xs(), n-1)
      }
  }

  def zindex[T](l: ZList[T], n: Int): T = {
    zhd(zdrop(l,n))
  }

  def zappend[T](l1: ZList[T], l2: ZList[T]): ZList[T] = {
    l1 match {
      case ZNil => l2
      case ZNode(x, xs) => ZNode(x, u=>zappend(xs(), l2))
    }
  }

  val zmul_three: ZList[Int] =
    ZNode(3, u=>zadd(zmul_three, 3))

  val zzeros: ZList[Int] = ZNode(0, u=>zzeros)

  val zzeros_ones: ZList[Int] = ZNode(0, u=>aux_zzeros_ones)
  val aux_zzeros_ones: ZList[Int] = ZNode(1, u=>zzeros_ones)

  val zzeros_ones_prof: ZList[Int] = ZNode(0, u=>ZNode(1, u=>zzeros_ones_prof))

  val zfib: ZList[Int] = ZNode(0, u=>ZNode(1, u=>aux_xfib(zfib)))
  def aux_xfib(l: ZList[Int]): ZList[Int] = {
    ZNode(zhd(l)+zhd(ztl(l)), u=>aux_xfib(ztl(l)))
  }
}
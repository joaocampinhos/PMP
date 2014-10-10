object ZList {
  private def build[T](l: List[T]): ZList[T] = {
    if( l.isEmpty ) ZNil
    else ZList(l.head, build(l.tail))
  }
  def apply[T](xs: T*) = {    // variable-length list of parameters 
    build(xs.toList)
  } 
  def apply[T](hd: T, tl: =>ZList[T]) = {
    lazy val v = tl             // this is a call-by-need "cache"
    new ZNode(hd, (u:Unit)=>v)  // ZNode is used only here
  }
}

abstract class ZList[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: ZList[T]
  def length: Int = {
    if (isEmpty) 0
    else 1 + tail.length
  }
  def map[U](f: T=>U): ZList[U] = {
    if (isEmpty) ZNil
    else ZList(f(head), tail.map(f))
  }
  def show(n: Int): List[T] = {
    if (n == 0 || isEmpty) Nil
    else head::(tail.show(n-1))
  }
  def show10: List[T] = show(10)
}

object ZNil extends ZList[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = sys.error("head: head of empty list")
  def tail: ZList[Nothing] = sys.error("tail: tail of empty list")
}

class ZNode[T](hd: T, tl: Unit=>ZList[T]) extends ZList[T] {
  def head: T = hd
  def tail: ZList[T] = tl()
  def isEmpty: Boolean = false
}

object Test extends App {
  val ints: ZList[Int] = ZList(1, ints.map(x=>x+1))
  println(ints.show10)
}




abstract class ZTree[+T]{
  def isEmpty: Boolean
  def root: T
  def left: ZTree[T]
  def right: ZTree[T]
  def map[R](f: T=>R): ZTree[R] = {
    if(isEmpty) ZNil
    else ZTree(f(root), left.map(f), right.map(f))
  }

//def findone(f: T=>Boolean): Option[T]
}
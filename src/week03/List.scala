/**
 *
 */
package week03

/**
 * @author malte
 *
 */
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
  def get(n: Int) = {
    def getHelp(acc: Int, remaining: List[T]): T = {
      if (tail.isEmpty) throw new IndexOutOfBoundsException
      else if (acc == 0) head else getHelp(acc - 1, remaining.tail)
    }
    getHelp(n, this)
  }
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  //List(1,2) ) List.apply(1,2)
  val x: List[String] = Nil
  // def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
  // def apply[T]() = Nil
}
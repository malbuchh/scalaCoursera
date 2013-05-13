package week03
import week03._
object session {
  val t1 = new NonEmpty(3, Empty, Empty)          //> t1  : week03.NonEmpty = {.3.}
  val t2 = t1 incl 4                              //> t2  : week03.IntSet = {.3{.4.}}
  // Lists
  
  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> list  : week03.Cons[Int] = week03.Cons@3f75c03e
  list.get(3)                                     //> res0: Int = 1
}

abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

object Empty extends IntSet {
	def contains(x: Int): Boolean = false
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
	def union(other: IntSet): IntSet = other
	override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def contains(x: Int): Boolean = if (x < elem) left contains x
	else if (x > elem) right contains x
	else true
	def incl(x: Int) : IntSet =
		if (x<elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
	def union(other: IntSet): IntSet = (left union right) union other incl elem
	override def toString = "{" + left + elem + right+ "}"
}
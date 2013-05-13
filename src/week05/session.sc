package week05

object session {
  val xs = List(1,2,3,4,5,6,7)                    //> xs  : List[Int] = List(1, 2, 3, 4, 5, 6, 7)
  xs.length                                       //> res0: Int = 7
  xs.last                                         //> res1: Int = 7
  xs.init                                         //> res2: List[Int] = List(1, 2, 3, 4, 5, 6)
  xs take 2                                       //> res3: List[Int] = List(1, 2)
  xs drop 2                                       //> res4: List[Int] = List(3, 4, 5, 6, 7)
  xs(0)                                           //> res5: Int = 1
  val ys = List(8,9)                              //> ys  : List[Int] = List(8, 9)
  xs ++ ys                                        //> res6: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  xs.reverse                                      //> res7: List[Int] = List(7, 6, 5, 4, 3, 2, 1)
  xs.updated (0,10)                               //> res8: List[Int] = List(10, 2, 3, 4, 5, 6, 7)
  xs indexOf 7                                    //> res9: Int = 6
  xs contains 10                                  //> res10: Boolean = false
  
  def init[T](xs: List[T]): List[T] = xs match {
  	case List() => throw new Error("init of empty list")
  	case List(x) => List()
  	case y :: ys => y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]
  def concat[T](xs: List[T], ys: List[T]): List[T] =
  	xs match {
  	case List() => ys
  	case z :: zs => z :: concat(zs,ys)
  }                                               //> concat: [T](xs: List[T], ys: List[T])List[T]
  def reverse[T](xs: List[T]) : List[T] = xs match {
  	case List() => xs
  	case y :: ys => reverse(ys) ++ List(y)
  }                                               //> reverse: [T](xs: List[T])List[T]
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)
                                                  //> removeAt: [T](n: Int, xs: List[T])List[T]
  def flatten(xs: List[Any]): List[Any] = xs match {
	  case Nil => Nil
  	case Nil :: xs => flatten(xs)
  	case (z :: zs) :: xs => z :: flatten(zs :: xs)
  }                                               //> flatten: (xs: List[Any])List[Any]
  removeAt[Int](2,xs)                             //> res11: List[Int] = List(1, 2, 4, 5, 6, 7)
  init[Int](xs)                                   //> res12: List[Int] = List(1, 2, 3, 4, 5, 6)
  
   def merge(xs: List[Int], ys: List[Int]): List[Int] = {
  xs match {
  	case Nil =>
  			 ys
  			case x :: xs1 =>
  			ys match {
  			case Nil =>
  			xs
  			case y :: ys1 =>
  			if (x < y) x :: merge(xs1,ys)
  			else y :: merge(xs,ys1)
  			}
  		}
  		 }                                //> merge: (xs: List[Int], ys: List[Int])List[Int]
  		 
  def msort(xs: List[Int]):List[Int] = {
  	val n = xs.length /2
  	if(n==0) xs
  	else {
  	def mergeP(xs: List[Int], ys: List[Int]): List[Int] = {
				(xs,ys) match {
				case (Nil,ys) => ys
				case (xs, Nil) => xs
				case (x :: xs1,y :: ys1) => if(x < y) x:: mergeP(xs1,ys) else y:: mergeP(xs,ys1)
  		}
  		}
  	val (fst, snd) = xs splitAt n
  	mergeP(msort(fst), msort(snd))
  	}
  }                                               //> msort: (xs: List[Int])List[Int]
  
 msort(List(1,2,1,7,8,1))                         //> res13: List[Int] = List(1, 1, 1, 2, 7, 8)
}
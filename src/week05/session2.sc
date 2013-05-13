package week05
import math.Ordering

object mergesort {
	def msort[T](xs: List[T])(implicit ord: Ordering[T]):List[T] = {
		val n = xs.length / 2
		if(n==0) xs
		else {
			def merge(xs: List[T], ys: List[T]): List[T] = (xs,ys) match {
				case (Nil,ys) => ys
				case (xs,Nil) => xs
				case (x :: xs1, y :: ys1) =>
					if(ord.lt(x,y)) x :: merge(xs,ys1)
					else y :: merge(xs,ys1)
			}
			
			val (fst, snd) = xs splitAt n
			merge(msort(fst),msort(snd))
			
		}
	}                                         //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

	val nums = List(2,-4,5,7,1)               //> nums  : List[Int] = List(2, -4, 5, 7, 1)
	msort(nums)                               //> res0: List[Int] = List(-4, -4, -4, -4, 2)
	val fruits = List("apple", "pineapple", "orange", "banana")
                                                  //> fruits  : List[String] = List(apple, pineapple, orange, banana)
	msort(fruits)                             //> res1: List[String] = List(apple, apple, apple, apple)
	
	def squareLists(xs: List[Int]): List[Int] = xs match {
		case Nil => Nil
		case y :: ys => y*y :: squareLists(ys)
	}                                         //> squareLists: (xs: List[Int])List[Int]
	
	def squareListsM(xs:List[Int]):List[Int] = xs map (x => x*x)
                                                  //> squareListsM: (xs: List[Int])List[Int]
  nums filter (x => x > 0)                        //> res2: List[Int] = List(2, 5, 7, 1)
  nums filterNot (x => x > 0)                     //> res3: List[Int] = List(-4)
	nums partition (x => x > 0)               //> res4: (List[Int], List[Int]) = (List(2, 5, 7, 1),List(-4))
	
	nums takeWhile( x => x > 0)               //> res5: List[Int] = List(2)
	nums dropWhile( x => x > 0)               //> res6: List[Int] = List(-4, 5, 7, 1)
	nums span(x => x > 0)                     //> res7: (List[Int], List[Int]) = (List(2),List(-4, 5, 7, 1))
	
	val packList = List("a","a","a","b","c","c","a")
                                                  //> packList  : List[String] = List(a, a, a, b, c, c, a)
	
	def pack[T](xs: List[T]): List[List[T]] = xs match {
	case Nil => Nil
	case x :: xs1 => val (first,rest) = xs span(y => y == x)
	first :: pack(rest)
	}                                         //> pack: [T](xs: List[T])List[List[T]]
	
	pack(packList)                            //> res8: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a)
                                                  //| )
	
	def encode[T](xs: List[T]): List[(T,Int)] = pack(xs).map(x=> (x.head, x.length))
                                                  //> encode: [T](xs: List[T])List[(T, Int)]
	encode(packList)                          //> res9: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
	
	def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( (x,y) => f(x) :: y )//> mapFun: [T, U](xs: List[T], f: T => U)List[U]

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((x,y) => y+1)                //> lengthFun: [T](xs: List[T])Int
    
  lengthFun[String](List("bla","blubb"))          //> res10: Int = 2
  
  mapFun[Int, Int](List(1,2,3),x => x+1)          //> res11: List[Int] = List(2, 3, 4)
  
  def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)
                                                  //> isPrime: (n: Int)Boolean
  isPrime(6)                                      //> res12: Boolean = false
	}
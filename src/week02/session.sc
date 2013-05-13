package week02
import math.abs
object session {
  def sum(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) 0 else f(a) + sum(f)(a+1,b)    //> sum: (f: Int => Int)(a: Int, b: Int)Int
  	
  def product(f: Int => Int)(a: Int, b: Int): Int =
  	if (a>b) 1 else f(a) * product(f)(a+1,b)  //> product: (f: Int => Int)(a: Int, b: Int)Int
  
  product(x=>x)(1,5)                              //> res0: Int = 120
  
  def fact(n: Int) =   	product(x=>x)(1,n)        //> fact: (n: Int)Int
  
  fact(3)                                         //> res1: Int = 6
  
  def combined(f: Int => Int)(combine: (Int, Int) => Int)(emptyBound: Int, a: Int, b: Int): Int =
  	if (a>b) emptyBound else combine(f(a), combined(f)(combine)(emptyBound,a+1,b))
                                                  //> combined: (f: Int => Int)(combine: (Int, Int) => Int)(emptyBound: Int, a: In
                                                  //| t, b: Int)Int
  combined(x=>x)((x,y) => x*y)(1,1,5)             //> res2: Int = 120
  
  val tolerance = 0.0001                          //> tolerance  : Double = 1.0E-4
  def isCloseEnough(x: Double, y: Double) =
   abs((x-y) / x) / x < tolerance                 //> isCloseEnough: (x: Double, y: Double)Boolean
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate(guess: Double): Double = {
  		val next = f(guess)
  		if(isCloseEnough(guess, next)) next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  fixedPoint(x=>1+x/2)(1)                         //> res3: Double = 1.999755859375
  def averageDamp(f: Double => Double)(x: Double) = (x+f(x))/2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
  def sqrt(x: Double) =
  	fixedPoint(averageDamp(y => x/y))(1)      //> sqrt: (x: Double)Double
  
  val x = new Rational(1,3)                       //> x  : week02.Rational = 1/3
  val y = new Rational(5,7)                       //> y  : week02.Rational = 5/7
  val z = new Rational(3,2)                       //> z  : week02.Rational = 3/2
  
  (x.sub(y)).sub(z)                               //> res4: week02.Rational = -79/42
  
  x.less(y)                                       //> res5: Boolean = true
  
  new Rational(2)                                 //> res6: week02.Rational = 2/1

}


class Rational(x: Int, y: Int) {
	require(y != 0, "denominator must be non-zero")
	
	def this(x: Int) = this(x,1)
	
	private def gcd(a: Int, b: Int): Int = if ( b == 0) a else gcd(b,a%b)
	private val g = gcd(x,y)
	def numer = x
	def denom = y
	
	def less(that: Rational) = this.numer* that.denom < that.numer * this.denom
	
	def max(that: Rational) = if(this.less(that)) that else this
	
	def add(that: Rational) =
		new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

	def neg = new Rational(-x,y)
	
	def sub(that: Rational) = add(that.neg)

	override def toString = numer/g + "/" + denom/g

}
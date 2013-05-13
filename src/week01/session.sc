package week01

object session {
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = abs(guess * guess - x) < x / 1000

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double
  sqrt(0.001)                                     //> res0: Double = 0.03162278245070105
  sqrt(0.1e-20)                                   //> res1: Double = 3.1633394544890125E-11
  sqrt(1.0e20)                                    //> res2: Double = 1.0000021484861237E10
  sqrt(1.0e50)                                    //> res3: Double = 1.0000003807575104E25
  sqrt(4)                                         //> res4: Double = 2.000609756097561
  
  def factorial(n: Int): Int = {
  	def loop(acc: Int, n: Int): Int =
  		if (n == 0) acc
  		else loop(acc*n,n-1)
  		loop(1,n)
  }                                               //> factorial: (n: Int)Int

	factorial(4)                              //> res5: Int = 24
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) return 1
    else if (coins.isEmpty || money < 0) return 0
    else  countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  countChange(4, List(1,2))                       //> res6: Int = 3
  countChange(300,List(5,10,20,50,100,200,500))   //> res7: Int = 1022
  countChange(301,List(5,10,20,50,100,200,500))   //> res8: Int = 0
  countChange(300,List(500,5,50,100,20,200,10))   //> res9: Int = 1022
  countChange(-50,List(1))                        //> res10: Int = 0
  countChange(100,List(-1,-2))                    //> java.lang.StackOverflowError
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfun$main$1.countChange$1(week01.session.scala:34)
                                                  //| 
                                                  //| 	at week01.session$$anonfu
                                                  //| Output exceeds cutoff limit.
	
}
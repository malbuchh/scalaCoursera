package week06

object session {
	def isPrime(n: Int) = (2 until n) forall (n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean
	val n = 7                                 //> n  : Int = 7
	((1 until n) flatMap ( i =>
		(1 until i) map ( j => (i,j)))) filter (pair => isPrime(pair._1 + pair._2))
                                                  //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
   def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
   	(for((x,y) <- xs zip ys)	yield (x* y)).sum
   }                                              //> scalarProduct: (xs: List[Double], ys: List[Double])Double
   
   def queens(n: Int): Set[List[Int]] = {
   	def placeQueens(k: Int): Set[List[Int]] = {
   		if(k==0) Set(List())
   		else
   			for{
   				queens <- placeQueens(k-1)
   				col <- 0 until n
   				if isSafe(col, queens)
   			} yield col :: queens
   	}
   	placeQueens(n)
   }                                              //> queens: (n: Int)Set[List[Int]]
   
   def isSafe(col: Int, queens: List[Int]): Boolean = {
   	val row = queens.length
   	val queensWithRow = (row - 1 to 0 by -1) zip queens
		queensWithRow forall {
		case(r,c) => col != c && math.abs(col - c) != row - r
		}
   }                                              //> isSafe: (col: Int, queens: List[Int])Boolean
   
   def show(queens: List[Int]) = {
   	val lines =
   		for (col <- queens.reverse)
   		yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
   		"\n" + (lines mkString "\n")
   }                                              //> show: (queens: List[Int])String
   
   queens(8) take 3 map show                      //> res1: scala.collection.immutable.Set[String] = Set("
                                                  //| * * * * * X * * 
                                                  //| * * * X * * * * 
                                                  //| * X * * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * * * X * * * 
                                                  //| * * * * * * X * 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * ", "
                                                  //| * * * * X * * * 
                                                  //| * * * * * * X * 
                                                  //| * X * * * * * * 
                                                  //| * * * X * * * * 
                                                  //| * * * * * * * X 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * X * * ", "
                                                  //| * * * * * X * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * * * X * * * * 
                                                  //| X * * * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * X * * * * * * 
                                                  //| * * * * X * * * ")
   
   val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
                                                  //> romanNumerals  : scala.collection.immutable.Map[String,Int] = Map(I -> 1, V
                                                  //|  -> 5, X -> 10)
   val capitals = Map("US" -> "Washington", "Switzerland" -> "Bern")
                                                  //> capitals  : scala.collection.immutable.Map[String,String] = Map(US -> Washi
                                                  //| ngton, Switzerland -> Bern)
   capitals get "andorra"                         //> res2: Option[String] = None
 	 capitals get "US"                        //> res3: Option[String] = Some(Washington)


	val fruit = List("apple", "pear", "orange", "pineapple")
                                                  //> fruit  : List[String] = List(apple, pear, orange, pineapple)
	fruit sortWith (_.length < _.length)      //> res4: List[String] = List(pear, apple, orange, pineapple)
	fruit.sorted                              //> res5: List[String] = List(apple, orange, pear, pineapple)
	fruit groupBy (_.head)                    //> res6: scala.collection.immutable.Map[Char,List[String]] = Map(p -> List(pea
                                                  //| r, pineapple), a -> List(apple), o -> List(orange))
}
package in.nene.d.ninetynine

import scala.math.sqrt
import scala.math.ceil

object Solution31 {
  def isPrime(n: Int) =
    (2 to (ceil(sqrt(n)) toInt)) . filter(_ != n) . forall (n % _ != 0)
}

object Solution32 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}

object Solution33 {
  def areCoprime(a: Int, b: Int) = Solution32.gcd(a,b) == 1
}

object Solution34 {
  def totient(n: Int) =
    (1 to n) filter(Solution33.areCoprime(n,_)) length
}

object Solution35 {
  def primeFactors(n: Int): List[Int] =
    if (n <= 1) List[Int]()
    else {
      val factors = (2 to (ceil(sqrt(n)) toInt)) . filter(_ != n) . filter (n % _ == 0)
      if ((factors length) > 0) (factors head) :: primeFactors(n/(factors head))
      else List[Int](n)
    }
}

object Solution36 {
  def primeFactorsMultiplicity(n: Int) : Map[Int,Int] =
    (Map[Int,Int]() /: Solution35.primeFactors(n)) ((m,k) => m + (k -> (m.getOrElse(k,0) + 1)))
}




package in.nene.d.ninetynine

object Solution21 {
  def insertAt[T](n: Int, v: T, l: List[T]): List[T] = 
    insertAt(n, v, l, Nil)
  private def insertAt[T](n: Int, v: T, l: List[T], a: List[T]): List[T] = {
    if (n == 0) ((v :: a) reverse) ::: l
    else l match {
      case Nil => a reverse
      case h :: t => insertAt(n - 1, v, t, h :: a)
    }
  }
}

object Solution22 {
  def range(start: Int, end: Int): List[Int] = 
    range(start, end, Nil)
  private def range(current: Int, end: Int, l: List[Int]): List[Int] =
    if (current <= end) range(current + 1, end, current :: l)
    else l reverse
}

import scala.util.Random

object Solution23 {
  val rand = new Random(System.currentTimeMillis())
  def getRandomElements[T](n: Int, l: List[T]): List[T] =
    getRandomElements(n, l, Nil)
  private def getRandomElements[T](n: Int, l: List[T], a: List[T]): List[T] = {
    if (n == 0 || ((l length) == 0)) a 
    else {
      val removed = Solution20.removeKth(rand.nextInt(l length), l) 
      removed match {
	case Right((r,v)) => getRandomElements(n - 1, r, v :: a)
	// Ideally this is not expected to happen
	// However I'm not raising an exception - your preference may vary
	case Left(r) => getRandomElements(n - 1, r, a)
      }
    }
  }
}

object Solution24 {
  def getRandomElements(n: Int, from: Int, to: Int) =
    Solution23.getRandomElements(n,List.range(from,to+1))
}

object Solution25 {
  def getRandomPermutation[T](l: List[T]) =
    Solution23.getRandomElements(l length,l)
}


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

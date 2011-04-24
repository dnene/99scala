package in.nene.d.ninetynine

object Solution11 {
  def encodeModified[T](list: List[T]): List[Either[(Int,T),T]] =
    Solution10.encode(list) map(x => if (x._1 == 1) Right(x._2) else Left(x))
}

object Solution12 {
  def decode[T](list: List[(Int,T)]): List[T] = 
    list flatMap (x => List.fill(x._1)(x._2))
}

object Solution13 {
  def encode[T](list: List[T]): List[(Int,T)] =
    encode(list, None, 0, List[(Int,T)]())
  private def encode[T](list: List[T], 
			last: Option[T], 
			counter: Int, 
			encoded: List[(Int,T)]): List[(Int,T)] = {
    list match {
      case Nil => 
	last match {
	  case None => encoded reverse
	  case Some(v) => ((counter, v) :: encoded) reverse
	}
      case h :: t =>
	last match {
	  case None => encode(t,Some(h),1,encoded)
	  case Some(v) => if (h == v) encode(t,last,counter+1,encoded)
			  else encode(t,Some(h),1,(counter,v) :: encoded)
	}
    }
  }
}

object Solution14 {
  def duplicate[T](l: List[T]) = l flatMap(List.fill(2)(_))
}

object Solution15 {
  def repeat[T](n: Int, l: List[T]) = l flatMap(List.fill(n)(_))
}

object Solution16 {
  def dropN[T](n: Int, l: List[T]): List[T] = dropN(n, 1, l, List[T]())
  private def dropN[T](n: Int, c: Int, l: List[T], a: List[T]): List[T] =
    l match {
      case Nil => a reverse
      case h :: t => 
	n == c match {
	  case true => dropN(n,1,t,a)
	  case false => dropN(n, c + 1, t, h :: a)
	}
    }
} 
      
object Solution17 {
  def splitAt[T](n: Int, l: List[T]):(List[T],List[T]) = 
    splitAt(3,0,l,List[T](),List[T]())
  private def splitAt[T](n: Int, 
			 c: Int, 
			 l: List[T], 
			 first: List[T], 
			 second: List[T]): (List[T],List[T]) = {
    l match {
      case Nil => ((first reverse),(second reverse))
      case h :: t => if (c < n) splitAt(n, c+1, t, h :: first, second)
		     else splitAt(n, c+1, t, first, h :: second)
    }
  }
}

object Solution18 {
  def slice[T](start: Int, finish: Int, list: List[T]) : List[T] =
    slice(start, finish, list, 0, List[T]())
  private def slice[T](s: Int, f: Int, l: List[T], c: Int, a: List[T]): List[T] = 
    l match {
      case Nil => a reverse
      case h :: t => if (c < s) slice(s, f, t, c + 1, a)
		else if (c < f) slice(s, f, t, c + 1, h :: a)
		else a reverse
    }
}

object Solution19 {
  def rotate[T](n: Int, l: List[T]): List[T] = {
    val len = l length
    var ctr = n
    while(ctr < 0) ctr += len
    while(ctr >= len) ctr -= len
    rotate(ctr,0,l,List[T]())
  }
  private def rotate[T](n: Int, c: Int, l: List[T], a: List[T]): List[T] =
    l match {
      // move lists in case of overflow
      case Nil => rotate(n, c, a reverse, Nil)
      case h :: t =>
	n - c match {
	  case 0 => l ::: (a reverse)
	  case _ => rotate(n, c + 1, t, h :: a)
	}
    }
}

object Solution20 {
  def removeKth[T](k: Int, l: List[T]): Either[List[T], (List[T],T)] = removeKth(k, l, Nil)
  private def removeKth[T](k: Int, l: List[T], a: List[T]): Either[List[T],(List[T],T)] =
    l match {
      case Nil => Left(a reverse)
      case h :: t => if (k == 0) Right((a reverse) ::: t,h)
	        else removeKth(k - 1, t, h :: a)
    }
}



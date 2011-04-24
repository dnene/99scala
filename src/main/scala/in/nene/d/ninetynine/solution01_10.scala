package in.nene.d.ninetynine

object Solution01 {
  def last[T](list: List[T]): T = {
    list match {
      case x :: Nil => x
      case x :: y => last(y)
      case _ => throw new NoSuchElementException()
    }
  }
}

object Solution02 {
  def secondLast[T](list: List[T]): T = 
    list match {
      case Nil => throw new NoSuchElementException()
      case x :: Nil => throw new NoSuchElementException()
      case x :: y :: Nil =>  x
      case x :: y  => secondLast(y)
    }
}

object Solution03 {
  def kTh[T](i: Int, list: List[T]): T = 
    list match {
      case h :: t => if (i == 0) h else kTh(i - 1, t)
      case _ => throw new NoSuchElementException()
    }
}

object Solution04 {
  def count[T](list: List[T]): Int = 
    count(list, 0)

  def count[T](list: List[T], counter: Int): Int = 
    list match {
      case x :: y => count(y,counter + 1)
      case _ => counter
    }
}

object Solution05 {
  def reverse[T](list: List[T]) : List[T] = reverse(list, List[T]())

  private def reverse[T](list: List[T], reversed: List[T]): List[T] = {
    list match {
      case h :: t => reverse(t, h :: reversed)
      case Nil => reversed
    }
  }
}

object Solution06 {
  def isPalindrome[T](list: List[T]): Boolean = {
    list match {
      case Nil => true
      case h :: Nil => true
      case h :: t => {
	if (h == (t last))
	  isPalindrome(t init)
	else
	  false
      }
    }
  }
}

object Solution07 {
  def flatten[T](nested: List[List[T]]): List[T] = 
    flatten(nested,List[T](),List[T]())
  private def flatten[T](nested: List[List[T]], 
			 current: List[T], 
			 flattened: List[T]): List[T] = {
    (nested, current) match {
      case (Nil, Nil) => flattened reverse
      case (_,  h:: t) => flatten(nested,t,h :: flattened)
      case (h :: t, Nil) => flatten(t, h, flattened)
    }
  }
}

object Solution08 {
  def compress[T](list: List[T]): List[T] = compress(list, None, List[T]())
  private def compress[T](list: List[T], 
		       last: Option[T], 
		       compressed: List[T]): List[T] = {
    list match {
      case Nil => compressed reverse
      case h :: t => {
	last match {
	  case None => compress(t, Some(h), h :: compressed)
	  case Some(v) => if (h == v) compress(t, last, compressed)
			    else compress(t, Some(h), h :: compressed)
	}
      }
    }
  }
}

object Solution09 {
  def pack[T](list: List[T]): List[List[T]] =
    pack(list,None,List[T](), List[List[T]]())
  private def pack[T](list: List[T],
		      last: Option[T],
		      current: List[T],
		      packed: List[List[T]]) : List[List[T]] = {
    list match {
      case Nil =>
	(current, packed) match {
	  case (Nil, Nil) => Nil
	  case _ => (current :: packed) reverse
	}
      case h :: t => 
	last match {
	  case None => pack(t, Some(h), List[T](h), packed)
	  case Some(v) => if (h == v) pack(t, last, h :: current, packed)
		     else pack(t, Some(h),List[T](h), current :: packed)
	}
    }
  }
}

object Solution10 {
  def encode[T](list: List[T]): List[(Int,T)] =
    Solution09.pack(list) map(x => (x.size, x.head))
}

import org.specs2.mutable._
import in.nene.d.ninetynine._

class Check99Spec extends Specification {
  def beBetween(i: Int, j: Int) = be_>=(i) and be_<=(j)

  "01. Find last item in a list" should {
    "Last item should be 8" in {
      Solution01.last(List(1,1,2,3,5,8)) must_== 8
    }

    "Last word is fox" in {
      Solution01.last(List("The", "quick", "brown", "fox")) must_== "fox"
    }

    "Last item in an empty list" in {
      Solution01.last(List[String]()) must throwA[NoSuchElementException]
    }
  }

  "02. Find last item in a list" should {
    "Second Last item should be 5" in {
      Solution02.secondLast(List(1,1,2,3,5,8)) must_== 5
    }

    "Second Last word is brown" in {
      Solution02.secondLast(List("The", "quick", "brown", "fox")) must_== "brown"
    }

    "Second Last item in an empty list" in {
      Solution02.secondLast(List[String]()) must throwA[NoSuchElementException]
    }

    "Second Last item in a single element list" in {
      Solution02.secondLast(List(5)) must throwA[NoSuchElementException]
    }
  }

  "03. Find k'th item in a list" should {
    "Check for first three items" in {
      Solution03.kTh(0,List(0,1,2,3,4)) must_== 0
      Solution03.kTh(1,List(0,1,2,3,4)) must_== 1
      Solution03.kTh(2,List(0,1,2,3,4)) must_== 2
      Solution03.kTh(3,List(0,1,2,3,4)) must_== 3
      Solution03.kTh(4,List(0,1,2,3,4)) must_== 4
      Solution03.kTh(5,List(0,1,2,3,4)) must throwA[NoSuchElementException]
    }

    "Any Element in an empty list" in {
      Solution03.kTh(0,List[String]()) must throwA[NoSuchElementException]
    }
  }

  "04. Count elements in a list" should {
    "Check counts" in {
      Solution04.count(List[Int]()) must_== 0
      Solution04.count(List(1)) must_== 1
      Solution04.count(List(1,2)) must_== 2
      Solution04.count(List(1,2,3)) must_== 3
    }
  }

  "05. Reverse a list" should {
    "Reverse an empty list" in {
      Solution05.reverse(List()) must_== List()
    }

    "Reverse a simple list" in {
      Solution05.reverse(List(1,2,3)) must_== List(3,2,1)
    }
  }

  "06. Check if a list is palindrome" should {
    "Treat an empty list as a palindrome" in {
      Solution06.isPalindrome(Nil) must_== true
    }

    "Treat a single element list as a palindrome" in {
      Solution06.isPalindrome(List(1)) must_== true
    }

    "Correctly treat an odd element palindrome list" in {
      Solution06.isPalindrome(List(1,2,3,2,1)) must_== true
    }

    "Correctly treat an even element palindrome list" in {
      Solution06.isPalindrome(List(1,2,3,3,2,1)) must_== true
    }

    "Correctly identify an odd element non palindrome" in {
      Solution06.isPalindrome(List(1,2,3,3,2)) must_== false
    }

    "Correctly identify an even element non palindrome" in {
      Solution06.isPalindrome(List(1,2,3,4,2,1)) must_== false
    }
  }

  "07. Flatten a list" should {
    "Treat an empty list properly" in {
      Solution07.flatten(List[List[Int]]()) must_== List[Int]()
    }

    "Properly flatten a single element list with an empty list" in {
      Solution07.flatten(List[List[Int]](List[Int]())) must_== List[Int]()
    }
    "Properly flatten a multi element list with all empty list" in {
      Solution07.flatten(List[List[Int]](List[Int](),List[Int]())) must_== List[Int]()
    }

    "Properly flatten a multi element list with multi elements" in {
      Solution07.flatten(List(List(1,2,3),List(4,5,6))) must_== List(1,2,3,4,5,6)
    }
  }

  "08. Eliminate consecutive duplicates" in {
    "Correctly treat an empty list" in {
      Solution08.compress(Nil) must_== Nil
    }
    "Reduce a list with identical elements to a single value" in {
      Solution08.compress(List(1,1,1,1,1,1,1)) must_== List(1)
    }
    "Reduce a list with three series of duplicates to a list with three elems" in {
      Solution08.compress(List(1,1,2,2,1,1)) must_== List(1,2,1)
    }
    "Remove all consecutive duplicates" in {
      Solution08.compress(
	List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== 
	  List('a, 'b, 'c, 'a, 'd, 'e)
    }
  }

  "09. Pack consecutive duplicates into sublists" in {
    "Empty list should return an empty list " in {
      Solution09.pack(List[Int]()) must_== List[List[Int]]()
      Solution09.pack(Nil) must_== Nil
    }

    "Single item list should return a single item list of list" in {
      Solution09.pack(List(1)) must_== List(List(1))
    }

    "Multiple item list with same values should return a single item list" in {
      Solution09.pack(List(1,1,1,1)) must_== List(List(1,1,1,1))
    }

    "Should process large list correctly" in {
      Solution09.pack(
	List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_==
	  List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    }
  }

  "10. Run length encoding of list" in {
    "Should process large list correctly" in {
      Solution10.encode(
	List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== 
	  List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }
  }

  "11. Modified run length encoding" in {
    "Should process large list correctly" in {
      Solution11.encodeModified(
	List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_==
	  List(Left(4,'a), Right('b), Left(2,'c), Left(2,'a), Right('d), Left(4,'e))
    }
  }

  "12. Run length decode a list" in {
    "Should process large list correctly" in {
      Solution12.decode(
	List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) must_==
	  List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    }
  }

  "13. Run length encoding of list" in {
    "Should process large list correctly" in {
      Solution13.encode(
	List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== 
	  List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }
  }

  "14. Duplicate the elements of a list" in {
    "Should duplicate all elements" in {
      Solution14.duplicate(List('a,'b, 'c, 'c, 'd)) must_==
	List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    }
  }

  "15. Repeat the elements a given number of times" in {
    "Should appropriate repeat all elements" in {
      Solution15.repeat(3, List('a, 'b, 'c, 'c, 'd)) must_== 
	List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    }
  }

  "16. Drop every n elements from a list" in {
    "Should drop every third element" in {
      Solution16.dropN(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
	List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    }
  }

  "17. Split list at given element" in {
    "Split list appropriately" in {
      Solution17.splitAt(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
	(List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }
  }

  "18. Extract a slice from the list" in {
    "Extract slice 3 thru 7" in {
      Solution18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
	List('d, 'e, 'f, 'g)
    }
  }

  "19. Rotate a list N places to the left" in {
    "Simple Rotation" in {
      Solution19.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
	List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    }
    "Negative value rotation" in {
      Solution19.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
	List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    }
  }

  "20. Remove K'th element from the list" in {
    "Simple removal" in {
      Solution20.removeKth(1, List('a, 'b, 'c, 'd)) must_==
	Right(List('a, 'c, 'd),'b)
    }
    "Too small a list" in {
      Solution20.removeKth(5, List('a, 'b, 'c, 'd)) must_==
	Left(List('a, 'b, 'c, 'd))
    }
  }

  "21. Insert element at given position" in {
    "Add at beginning" in {
      Solution21.insertAt(0, 5, List(1,2,3,4)) must_== List(5,1,2,3,4)
    }

    "Add in the middle" in {
      Solution21.insertAt(2, 5, List(1,2,3,4)) must_== List(1,2,5,3,4)
    }

    "Add at the end" in {
      Solution21.insertAt(4, 5, List(1,2,3,4)) must_== List(1,2,3,4,5)
    }

    "Add beyond the end" in {
      Solution21.insertAt(5, 5, List(1,2,3,4)) must_== List(1,2,3,4)
    }
  }

  "22. Create a list containing integers in a range" in {
    "End lesser than start" in {
      Solution22.range(2,1) must_== Nil
    }

    "Start same as end" in {
      Solution22.range(1,1) must_== List(1)
    }

    "Usual range" in {
      Solution22.range(4, 9) must_== List(4, 5, 6, 7, 8, 9)
    }
  }

  "23. Get random elements from a list" in {
    "Get no random elements" in {
      Solution23.getRandomElements(0, List(1,2,3)) must_== Nil
    }
    "Get one random element" in {
      Solution23.getRandomElements(1, List(1,2,3)) must beOneOf(List(1),List(2),List(3))
    }
    "Get few random elements" in {
      val r = Solution23.getRandomElements(3,List(1,2,3,4,5,6))
      r must have size(3)
      (r toSet) must have size(3)
      r must (be_>=(1) and be_<=(6)).forall
    }
    "Request more random elements than elements in list" in {
      val r = Solution23.getRandomElements(9,List(1,2,3,4,5,6))
      r must have size(6)
      (r toSet) must have size(6)
      r must (be_>=(1) and be_<=(6)).forall
    }
  }
  "24. Get random elements from a range" in {
    "Get no random elements" in {
      Solution24.getRandomElements(0,1,3) must_== Nil
    }
    "Get one random element" in {
      Solution24.getRandomElements(1, 1, 3) must beOneOf(List(1),List(2),List(3))
    }
    "Get few random elements" in {
      val r = Solution24.getRandomElements(3,1,6)
      r must have size(3)
      (r toSet) must have size(3)
      r must (be_>=(1) and be_<=(6)).forall
    }
    "Request more random elements than elements in list" in {
      val r = Solution24.getRandomElements(9,1,6)
      r must have size(6)
      (r toSet) must have size(6)
      r must (be_>=(1) and be_<=(6)).forall
    }
  }

  "25. Get random permutations from a list" in {
    "Get random permutations" in {
      val r = Solution25.getRandomPermutation(List(1,2,3,4,5,6))
      r must have size(6)
      (r toSet) must have size(6)
      r must (be_>=(1) and be_<=(6)).forall
    }
  }

  "26. Get all combinations" in {
    "Get combinations" in {
      val c = Solution26.combinations(3,List(1,2,3,4,5,6,7,8,9,10,11,12))
      c must have size(220)
    }
  }

  "27. Group elements into disjoint subsets" in {
    "Group 3 elements" in {
      var r = Solution27.group(List(1,2),List(1,2,3))
      // expected = [[[1],[2,3]],[[2],[1,3]],[[3],[1,2]]]
      r must have size(3)
      (r toSet) must have size(3)
      // more checks to be added
    }
  }

  "28a. Sort List of Lists by Length" in {
    "simple sort" in {
      Solution28a.sortByLength(List(List(1,2,3),List(4),List(5,6))) must_==(
	List(List(4),List(5,6),List(1,2,3)))
    }
  }

  "28b. Sort List of lists by frequency of occurrence" in {
    "do sort" in {
      Solution28b.freqSort(
	List(
	  List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), 
	  List('i, 'j, 'k, 'l), List('m, 'n), List('o))) must_== 
	    List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), 
		 List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
    }
  }

  "31. Test if number is prime" in {
    "2 is prime" in {
      Solution31.isPrime(2) must_== true
    }
    "7 is prime" in {
      Solution31.isPrime(7) must_== true
    }
    "9 is not prime" in {
      Solution31.isPrime(9) must_== false
    }
    "12 is not prime" in {
      Solution31.isPrime(12) must_== false
    }
  }

  "32. Find gcd of two numbers" in {
    "GCD of 36 and 63" in {
      Solution32.gcd(36,63) must_== 9
    }
  }

  "33. Are numbers coprime" in {
    "35 and 63 are not coprime" in {
      Solution33.areCoprime(35,63) must_== false
    }
    "35 and 64 are coprime" in {
      Solution33.areCoprime(35,64) must_== true
    }
  }

  "34. Compute totient" in {
    "Totient of 10 is 4" in {
      Solution34.totient(10) must_== 4
    }
  }

  "35. Compute prime factors" in {
    "Prime factors of 315" in {
      Solution35.primeFactors(315) must_== List(3, 3, 5, 7)
    }
  }

  "36. Prime factors multiplicity" in {
    "Prime factor multiplicity of 315" in {
      Solution36.primeFactorsMultiplicity(315) must_== Map(3 -> 2, 5 -> 1, 7 -> 1)
    }
  }
}

package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("link1") = { 
    val h = insert(1, insert(2, empty))
    val h1 = insert(4, insert(5, h))
    val ord = toList(h1).sorted
    val ord1 = toList(h).sorted
    findMin(h1) == ord(0) && findMin(h) == ord1(0)
  }

  property("link2") = { 
    val h = insert(6, insert(2, empty))
    val h1 = insert(4, insert(5, h))
    val h2 = insert(8, empty)
    val h3 = meld(h1, h2)
    val ord = toList(h1) ::: toList(h2)
    val ord1 = toList(h).sorted
    /* println(s"h $h")
    println(s"h1 $h1")
    println(s"toList h ${toList(h)}")
    println(s"toList h1 ${toList(h1)}")
    println(s"h3 $h3")
    println(s"tolist h3 ${toList(h3)}")*/
    val l = List(2, 6) 
    val l1 = List(2,4,5,6)
    toList(h3) == ord
  }

  lazy val genHeap: Gen[H] = for { 
    v <- arbitrary[Int]
    // h <- oneOf(const(empty), genHeap)
    h <- frequency((1, const(empty)), (9, genHeap))
  }yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

   def toList(h: H): List[A] = h match {
       case h if isEmpty(h) => Nil
       case h => findMin(h) :: toList(deleteMin(h))
   }

}

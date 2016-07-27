package common

import quickcheck._

trait HeapListHelper extends Heap {
   def toList(h: H): List[A] = h match {
       case h if isEmpty(h) => Nil
       case h => findMin(h) :: toList(deleteMin(h))
   }
   
   def fromList(l: List[A]) = (l foldRight empty) (insert(_,_)) 
}

package selx

import scala.annotation.tailrec

object SelectHeap extends Select
{
  private[selx] def worstCase( i: Int, from: Int, until: Int ): Long =
  {
    val L: Long = until-i   max i-from+1
    val l: Long = until-from - L

    if( l == 0 ) return 0

    var       logL=31
    if({1L << logL-16} >= L) logL -= 16
    if({1L << logL- 8} >= L) logL -=  8
    if({1L << logL- 4} >= L) logL -=  4
    if({1L << logL- 2} >= L) logL -=  2
    if({1L << logL- 1} >= L) logL -=  1

    assert{ L <= (1L << logL  ) }
    assert{ L >  (1L << logL-1) }
    2*L + l*logL // heap building + dequeue
  }

  override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T, T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    if( i-from < until-i ) right(values,  compareFn,from,i,i,until)
    else                    left(values,  compareFn,from, i, until)
  }

  /** Builds a Max-Heap on the lest sied and exchanges with the right side.
    */
  private def left[@specialized T]( values: Array[T], compareFn: (T, T) => Int, from: Int, i: Int, until: Int ): Unit =
  {
    val firstBranch = i - (i-from-1 >>> 1)  ensuring  (_ >= from)

    @tailrec def siftDown( p: Int ): Unit
      = if( p >= firstBranch ) {
          assert( p <= i )
          var c = i-1 - (i-p << 1) ensuring (_ < until)
          assert( c >= from )
          assert( c < p )
          if( c > from && compareFn(values{c-1}, values{c}) > 0 )
              c -=1
          if( compareFn(values{c}, values{p}) > 0 ) {
            swap(values,p,c)
            siftDown(c)
          }
        }
    // build heap
    var        j = firstBranch
    while(     j <= i ) {
      siftDown(j)
               j += 1
    }
    // dequeue
            j = i
    while( {j+= 1; j} < until ) {
      if( compareFn(values{i}, values{j}) > 0 ) {
        swap(values,j,i)
        siftDown(i)
      }
    }
  }

  /** Builds a Min-Heap on the right side and exchanges with the left side.
    */
  private[selx] def right[@specialized T]( values: Array[T], compareFn: (T, T) => Int, lFrom: Int, lUntil: Int, rFrom: Int, rUntil: Int ): Unit =
  {
//    assert( lFrom <= lUntil )
//    assert(          lUntil <= rFrom )
//    assert(                    rFrom < rUntil )
//    assert( lUntil-lFrom <= rUntil-rFrom )

    val firstLeaf = rFrom+1 + (rUntil-rFrom-2 >>> 1)  ensuring  (_ < rUntil)

    @tailrec def siftDown( p: Int ): Unit
      = if( p < firstLeaf ) {
          assert( p >= rFrom )
          var c = 1+rFrom + (p-rFrom << 1)
          assert( c < rUntil)
          assert( c > p )
          if( c + 1 < rUntil && compareFn(values{c}, values{c+1}) > 0 )
              c +=1
          if( compareFn(values{p}, values{c}) > 0 ) {
            swap(values,p,c)
            siftDown(c)
          }
        }
    // build heap
    var        j = firstLeaf
    while(     j > rFrom ) {
               j-= 1
      siftDown(j)
    }
    // dequeue
           j = lUntil
    while( j > lFrom ) {
           j-= 1
      if( compareFn(values{j}, values{rFrom}) > 0 ) {
        swap(values,j,rFrom)
        siftDown(rFrom)
      }
    }
  }

//  private[selx] def apply[@specialized T]( values: Array[T], compareFn: (T, T) => Int,  lFrom: Int, lUntil: Int,  mid: Int,  rFrom: Int, rUntil: Int ): Unit =
//  {
//    assert( lFrom < lUntil )
//    assert(         lUntil <= mid )
//    assert(                   mid < rFrom )
//    assert(                         rFrom < rUntil )
//    assert( lUntil - lFrom  <=  rUntil - rFrom )
//
//    val firstLeaf = rFrom + (rUntil-rFrom-1 >>> 1)  ensuring  (_ < rUntil)  ensuring  (_ >= rFrom)
//
//    def siftDownRoot(): Unit = {
//      var c = rFrom
//      if( c + 1 < rUntil && compareFn(values{c}, values{c+1}) > 0 )
//          c +=1
//      if( compareFn(values{mid}, values{c}) > 0 ) {
//        swap(values,mid,c)
//        siftDown(c)
//      }
//    }
//
//    @tailrec def siftDown( p: Int ): Unit
//      = if( p < firstLeaf ) {
//          assert( p >= rFrom )
//          var c = rFrom + (p-rFrom+1 << 1)
//          assert( c < rUntil )
//          assert( c > p )
//          if(   1+c < rUntil && compareFn(values{c}, values{c+1}) > 0 ) c += 1
//          if(                   compareFn(values{p}, values{c  }) > 0 ) {
//            swap(values,p,c)
//            siftDown(c)
//          }
//        }
//
//    // build heap
//    var        j = firstLeaf
//    while(     j > rFrom ) {
//               j-= 1
//      siftDown(j)
//    }
//    siftDownRoot()
//
//    // dequeue
//           j = lUntil
//    while( j > lFrom ) {
//           j-= 1
//      if( compareFn(values{j}, values{mid}) > 0 ) {
//        swap(values,j,mid)
//        siftDownRoot()
//      }
//    }
//  }
}

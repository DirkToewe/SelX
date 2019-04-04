package selx

import java.lang.Math.abs

import scala.annotation.tailrec

/** A Selection Algorithm similar to Quick Select that recursively splits/pivotizes the array
  * around the mean value. The underlying assumption is that the average value is close enough
  * to the median to roughly split the data in half. If that assumption holds true, the
  * complexity should be O(n). The worst case is O(n²).
  *
  * On random uniform data, this algorithm is superior to both quick-select and median-of-medians.
  *
  * This Selection algorithm requires that each entry of the array input can be represented
  * as a `Double` value for comparison. It therefore has a different interface than the other
  * selection algorithms in this library.
  */
object MeanSelectV2
{
  def apply[@specialized T]( values: Array[T], toDouble: T => Double, i: Int ): Unit
    = apply(values,toDouble, i, 0,values.length)

  def apply[@specialized T]( values: Array[T], toDouble: T => Double, i: Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    val compareFn = (x: T, y: T) => { toDouble(x) - toDouble(y) }.signum

    @inline def vals( i: Int ): Double = toDouble( values(i) )

    @tailrec def select( from: Int, until: Int ): Unit = {
      val len = until - from ensuring (_ > 0)
      if( len <= 16 )
        SelectHeap(values,i,compareFn, from,until)
      else {
        var mean = 0.0
        var j = until; while( j > from ) { j -= 1; mean += vals(j) }
        mean /= len

//        // https://en.wikipedia.org/wiki/Kahan_summation_algorithm
//        var mean,rest = 0.0
//        var j = until; while( j > from ) { j -= 1
//          val a = vals(j) - rest
//          val b = a + mean
//          rest = (b - mean) - a
//          mean =  b
//        }
//        mean /= len

        var l = from
        var m = from+1
        var r = until
        var piv = vals(l)
        while( m < r )
        {
          val nxt = vals(m)
          if( nxt == piv ) // <- keep track of multiple pivots (allows us to adjust `split` to be closer to `i`)
            m += 1
          else if( abs(nxt-mean) < abs(piv-mean) ) { // <- found a pivot closer to the mean
            if( nxt < piv ) {
              swap(values,l,m)
              while( m > l ) {
                r -= 1; swap(values,m,r)
                m -= 1
              }
            }
            piv = nxt
            l = m; m += 1
          }
          else if( nxt > piv ) { // <- value right of split
            r -= 1
            swap(values,m,r)
          }
          else { // <- value left of split
            assert( nxt < piv )
            swap(values,l,m)
            l += 1
            m += 1
          }
        }
        assert( from <= l )
        assert(         l < m )
        assert(             m < until )
        assert(m==r)
        val split = i max l min r-1
        if( split > i ) select(from,    split) else
        if( split < i ) select(split+1, until)
      }
    }

    select(from,until)
  }
}

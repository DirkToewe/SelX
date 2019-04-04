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
object MeanSelectV1
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
      if( len <= 8 ) {
        var j = from; while( j <= i    ) {
        var k = j+1 ; while( k < until ) {
          if( vals(j) > vals(k) ) swap(values,j,k)
        k += 1 }
        j += 1 }
      }
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

        var pivVal = abs{ vals(j) - mean }
        var pivIdx =      j
        assert( j == from )
        while( {j+=1; j} < until ) {
          val          vj = abs{ vals(j) - mean }
          if( pivVal > vj ) {
              pivVal = vj
              pivIdx =  j
          }
        }
        val split = Partition[T](values,values(pivIdx), i, compareFn, from,until)
        if( split > i ) select(from,    split) else
        if( split < i ) select(split+1, until)
      }
    }

    select(from,until)
  }
}

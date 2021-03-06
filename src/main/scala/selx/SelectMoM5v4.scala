package selx

/** Median-of-Medians Selection Algorithm with a group size of 5.
  * This was the first known Selection Algorithm with a guaranteed
  * complexity of O(n). In practice however this algorithm is slower
  * than good sorting algorithms for all reasonable problem sized.
  */
object SelectMoM5v4 extends Select
{
  private[selx] def worstCase( i: Int, from: Int, until: Int ): Long
    = (until-from) * 10L / 3 // <- TODO: double-check

  override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    def select( i: Int, from: Int, until: Int ): Unit =
    {
      val len = until - from ensuring (_ >= 0)
      if( len <= 320 ) // <- len < 5 is lowest sensible bound
        SelectHeap(values,i, compareFn, from,until)
      else {
        // compute medians of 5-entry-groups
        var  k,j = from
        while( j < until-9 ) {
          Median5v1( values, compareFn, j, j+2, j+3 )
          swap(values,j+2,k)
          k += 1
          j += 5
        }
        // compute medians of last 5-9 entries
        var          mid = (j+until) >>> 1; SelectHeap(values,mid, compareFn, j,until)
        swap(values, mid, k)
                          k += 1
               mid =(from+k) >>> 1
        // compute median of medians
        select(mid,  from,k)
        // partition
        val                      piv = values(mid)
            j = Partition(values,piv,i, compareFn, from,until)
        if( j > i ) select(i, from, j    ) else
        if( j < i ) select(i, j+1,  until)
      }
    }

    select(i,from,until)
  }
}

package selx

/** Median-of-Medians Selection Algorithm with a group size of 5.
  * This was the first known Selection Algorithm with a guaranteed
  * complexity of O(n). In practice however this algorithm is slower
  * than good sorting algorithms for all reasonable problem sized.
  */
object SelectMoM3v2 extends Select
{
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
        val               tert =  len/3
        val lUntil = (len-tert)/2 + from
        val rFrom  =      tert + lUntil
        // compute medians of 3-entry-groups
        var    l =  from
        var    r =  until
        var    m = lUntil
        while( m < rFrom-1 ) {
          r -= 1; Median3(values, compareFn, l,m,r)
          l += 1
          m += 1
        }
        assert{ m+1 == rFrom }
//        assert(lUntil-l + r-m >= 3)
//        assert(lUntil-l + r-m <= 5)
        // compute median of last 3-5 entries
        SelectHeap.right(values,compareFn, l,lUntil, m,r)
        r = rFrom
        // compute median of medians
        var    mid = lUntil + tert/2
        select(mid,  lUntil, r)
        // partition right of the middle pentile
        while( r < until ) {
          if( compareFn(values{mid},  values{r}) > 0 ) {
            val tmp  =  values(mid)
                        values(mid) = values(r);         mid += 1
                                      values(r) = values(mid)
                                                  values(mid) = tmp
          }
          r += 1
        }
        // partition left of the middle pentile
               l = lUntil
        while( l > from ) {
          l-= 1
          if( compareFn(values{mid},  values{l}) < 0 ) {
            val tmp  =  values(mid)
                        values(mid) = values(l);         mid -= 1
                                      values(l) = values(mid)
                                                  values(mid) = tmp
          }
        }
        // choose the correct half to continue in
        if( mid > i ) select(i, from,  mid  ) else
        if( mid < i ) select(i, mid+1, until)
      }
    }

    select(i,from,until)
  }
}

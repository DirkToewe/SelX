package selx

/** Median-of-Medians Selection Algorithm with a group size of 5.
  * This was the first known Selection Algorithm with a guaranteed
  * complexity of O(n). In practice however this algorithm is slower
  * than good sorting algorithms for all reasonable problem sized.
  */
object SelectMoM5v7 extends Select
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
        val               pentil = len%5/3 + len/5
        val lUntil = (len-pentil)/2 + from
        val rFrom  =      pentil + lUntil
        // compute medians of 5-entry-groups
        var    l =  from
        var    r =  until
        var    m = lUntil
        while( m < rFrom-1 ) {
               r -= 2; Median5v1(values, compareFn, l,m,r)
               l += 2
               m += 1
        }
//        assert(lUntil-l + r-m >= 3)
//        assert(lUntil-l + r-m <= 7)
        // compute median of last 3-7 entries
        SelectHeap.right(values,compareFn, l,lUntil, m,r)
        r = m+1
//        assert( r == lUntil + pentil )
        // compute median of medians
        var    mid = lUntil + pentil/2
               mid = mid  min  lUntil + (i  - from)/3 // if `i` far left,  we can move mid to the left  safely
               mid = mid  max  rFrom  - (until-i+2)/3 // if `i` far right, we can move mid to the right safely
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

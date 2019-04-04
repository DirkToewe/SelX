package selx

/** Median-of-Medians Selection Algorithm with a group size of 5.
  * This was the first known Selection Algorithm with a guaranteed
  * complexity of O(n). In practice however this algorithm is slower
  * than good sorting algorithms for all reasonable problem sized.
  */
object SelectMoM3v3 extends Select
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
        val          tert = len/3
        val lUntil = tert + from
        // compute medians of 3-entry-groups
        var    l =  from
        var    r =  until
        var    m = lUntil
        while( l < lUntil ) {
          r -= 1; Median3(values, compareFn, l,m,r)
          l += 1
          m += 1
        }
        // compute median of medians
        var    mid = lUntil + tert/2
               mid = mid  min  lUntil + (i  - from)/2 // if `i` far left,  we can move mid to the left  safely
               mid = mid  max  m      - (until-i+1)/2 // if `i` far right, we can move mid to the right safely
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

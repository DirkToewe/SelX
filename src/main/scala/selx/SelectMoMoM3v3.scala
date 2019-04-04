package selx

/** An implementation of the Repeated Step Algorithm as described in:
  *
  * "Select with Groups of 3 or 4"
  * Ke Chen, Adrian Dumitrescu
  * October 21, 2015
  *
  * http://www.cs.uwm.edu/faculty/ad/select.pdf
  *
  * With further optimizations as described in:
  *
  * "Fast Deterministic Selection"
  * Andrei Alexandrescu
  * https://arxiv.org/abs/1606.00484
  */
object SelectMoMoM3v3 extends Select
{
  override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    def select( i: Int, from: Int, until: Int ): Unit =
    {
      assert( from <= i )
      assert(         i < until )

      var len = until - from ensuring (_ > 0)
//      if( len < 9*3 ) // <- len < 9 is lowest bound possible
      if( len < 256 ) // <- len < 9 is lowest bound possible
        SelectHeap(values,i, compareFn, from,until)
      else {
        var l = from
        var r = until
        var    run = 2
        while( run > 0 )
        {      run-= 1
                               len = r - l
          val           tert = len/3
          val lUntil = (len-tert)/2 + l
          val rFrom  =      tert    + lUntil

          assert( tert > 1 )
          var    m = lUntil
          while( m < rFrom-1 ) {
            r -= 1; Median3(values,compareFn, l,m,r)
            l += 1
            m += 1
          }
          // compute median of the last 3-5 values
          assert(lUntil-l + r-m >= 3)
          assert(lUntil-l + r-m <= 5)
          SelectHeap.right(values,compareFn, l,lUntil, m,r)
          l = lUntil
          r = rFrom
        }

        var    mid = l+r >>> 1
        select(mid,  l,r)

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

    select(i, from, until)
  }
}

package selx

/** An implementation of the Repeated Step Algorithm as described in:
  *
  * "Select with Groups of 3 or 4"
  * Ke Chen, Adrian Dumitrescu
  * October 21, 2015
  *
  * http://www.cs.uwm.edu/faculty/ad/select.pdf
  */
object SelectMoMoM3v2 extends Select
{
  override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    def select( i: Int, from: Int, until: Int ): Unit =
    {
      /** Splits the values into groups of three, finds the median of each group
        * and move all those medians to the beginning of the range (starting at from)
        * by swapping values.
        *
        * @param until The exclusive end of the range, sel is applied to.
        */
      @inline def sel3( until: Int ) = {
        // find the median for groups of three
        var  i,j = from
        while( j < until-5 ) {
          val ab = compareFn(values{j+0}, values{j+1}) <= 0
          val ac = compareFn(values{j+0}, values{j+2}) <  0

          if( ab != ac ) swap(values, i, j+0)
          else {
            val bc = compareFn(values{j+1}, values{j+2}) < 0
            if( ab == bc ) swap(values, i, j+1)
            else           swap(values, i, j+2)
          }
          i += 1
          j += 3
        }
        // find median of the last group of 3-5 values
        val               mid =          (j+until) >>> 1
        SelectHeap(values,mid, compareFn, j,until)
              swap(values,mid,i)
      }

      var len = until - from ensuring (_ > 0)
      if( len < 256 ) // <- len < 9 is lowest bound possible
        SelectHeap(values,i, compareFn, from,until)
      else {
        var k = 2; while( k > 0 ) { k -= 1; sel3(from + len); len /= 3 }

        val    mid   =   from+len/2
        select(mid, from,from+len)

        val                      piv = values(mid)
        val j = Partition(values,piv,i, compareFn, from,until)
        if( j > i ) select(i, from, j    ) else
        if( j < i ) select(i, j+1,  until)
      }
    }

    select(i, from, until)
  }
}

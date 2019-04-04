package selx

/** Selects the middle of 5 entries in an array.
  */
private[selx] object Median5v1
{
  @inline private[selx] def apply[@specialized T](values: Array[T], compareFn: (T,T) => Int, l: Int, i: Int, r: Int ): Unit =
  {
    //    assert( l+1 < i )
    //    assert(       i < r )
    // takes guaranteed 6 comparisons
    // see: https://arxiv.org/abs/1606.00484
    // TODO: investigate other median 5 approaches. There is one with 4 comparisons best-case. Maybe You can get that one to work in 6 comparisons worst-case
    @inline def less( i: Int, j: Int ) = compareFn( values(i), values(j) ) < 0
    @inline def a = l+0
    @inline def b = l+1
    @inline def c = i
    @inline def d = r+0
    @inline def e = r+1
    if( less(c,a) )   swap(values,c,a)
    // a <= c
    //   b       d e
    if( less(d,b) )   swap(values,d,b)
    // a <= c      e
    //   b  <=   d
    if( less(d,c) ) { swap(values,d,c);
                      swap(values,a,b) }
    // a <= c      e
    //      c <= d
    //   b  <=   d
    if( less(e,b) )   swap(values,e,b)
    // a <= c
    //      c <= d
    //   b  <=   d
    //   b    <=   e
    if( less(e,c) ) { swap(values,e,c)
      // a  b <= c  d >= e
      //         c  <=   e
      if( less(c,a) ) swap(values,c,a)
    }
    else {
      // a  b  c <= d
      // a  <= c   <= e
      if( less(c,b) ) swap(values,c,b)
    }
  }
}

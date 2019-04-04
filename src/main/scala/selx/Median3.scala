package selx

/** Sorts three entries in an array.
  */
private[selx] object Median3
{
  @inline private[selx] def apply[@specialized T](values: Array[T], compareFn: (T,T) => Int, l: Int, m: Int, r: Int ): Unit =
  {
//    assert( l < m )
//    assert(     m < r )
    var lm = compareFn(values{l}, values{m}).signum
    val mr = compareFn(values{m}, values{r}).signum

    if( lm*mr >= 0 ) {
      if( lm > 0 || mr > 0 ) swap(values,l,r)
    }
    else {
      val lr = compareFn(values{l}, values{r})
      if( lr > 0 ) {
        swap(values,l,r)
        lm = -mr
      }
      if( lm > 0 ) swap(values,l,m)
      else         swap(values,m,r)
    }
//    assert( compareFn(values{l}, values{m}) <= 0 )
//    assert( compareFn(values{m}, values{r}) <= 0 )
  }
}

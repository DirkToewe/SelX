package selx

private[selx] object Partition
{
  @inline def apply[@specialized T]( values: Array[T], pivot: T, split: Int, compareFn: (T,T) => Int, from: Int, until: Int ) =
  {
    assert( 0 <= from )
    assert(      from <= split )
    assert(              split < until )
    var    k = from
    var  j,i = until
    while( i > k ) {
           i-= 1
      val c = compareFn(pivot, values{i})
      if( c < 0 ) { j -= 1; swap(values,i,j) } else
      if( c ==0 ) {         swap(values,i,k); i += 1;
                                              k += 1 } // <- copy pivots to front
    }
    assert( k > from )
    do {
      j -= 1
      k -= 1; swap(values,j,k)
    }
    while( k > from && j > split ) // <- try to get as close as possible to the desired split index
    j
  }
}

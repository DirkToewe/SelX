package selx

/** A Selection Algorithm inspired by Bubble Sort.
  *
  * The complexity is `O( (until-i)*(i+1-from) )` which mean it is efficient for small arrays or
  * pivot indices close to the ends of the array.
  */
object SelectBubble extends Select
{
  @inline override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    var    k = i
    while( k < until ) {
      var    j = from
      while( j < i ) {
         if(          compareFn(values{j},values{k}) > 0 ) swap(values,j,k); j += 1
      }; if( i < k && compareFn(values{i},values{k}) > 0 ) swap(values,i,k); k += 1
    }
  }
}

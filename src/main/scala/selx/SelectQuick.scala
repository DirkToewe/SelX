package selx

import scala.annotation.tailrec

private[selx] class SelectQuick( choosePivot: PivotChooser ) extends Select
{
  override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    assert( 0 <= from )
    assert(      from <= i )
    assert(              i < until )
    assert(                  until <= values.length )

    @tailrec def select( from: Int, until: Int ): Unit =
    {
      val len = until - from ensuring (_ > 0)
      if( len < 48 )
        SelectHeap(values,i, compareFn, from,until)
      else {
        val                          pivot = choosePivot(values,i, compareFn, from,until)
        val split = Partition(values,pivot,i, compareFn, from,until)
        if( split > i ) select(from,    split) else
        if( split < i ) select(split+1, until)
      }
    }

    select(from,until)
  }
}

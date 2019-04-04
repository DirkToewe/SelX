package selx

private[selx] trait PivotChooser
{
  def apply[@specialized T]( array: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): T
}

package selx

/** Super-trait of most Selection Algorithms.
  */
trait Select
{
  def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int ): Unit
    = apply(values,i, compareFn, 0,values.length)

  def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit
}
object Select extends Select
{
  override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit
    = SelectMoMoM3v1(values,i, compareFn, from,until)
}

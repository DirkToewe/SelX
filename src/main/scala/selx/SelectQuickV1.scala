package selx

import scala.util.Random

object SelectQuickV1 extends Select
{
  private class PivotChooserV1 extends PivotChooser
  {
    private val rng = new Random(1337)

    override def apply[@specialized T]( array: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): T
      = array apply rng.nextInt(until-from)+from
  }

  override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    // create new RNG every time -> deterministic behaviour
    val select = new SelectQuick( new PivotChooserV1 )
    select(values,i, compareFn, from,until)
  }
}

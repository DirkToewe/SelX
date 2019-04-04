package selx

import scala.util.Random

object SelectQuickV2 extends Select
{
  private class PivotChooserV2 extends PivotChooser
  {
    private val rng = new Random(1337)

    override def apply[@specialized S]( values: Array[S], i: Int, compareFn: (S,S) => Int, from: Int, until: Int ): S =
    {
      assert(values.length > 2)

      val i = rng.nextInt(until-from-0) + from
      var j = rng.nextInt(until-from-1) + from
      var k = rng.nextInt(until-from-2) + from

      if( i <= j ) j += 1
      if( i <= k ) k += 1
      if( j <= k ) k += 1

      val a = values(i)
      val b = values(j)
      val c = values(k)

      var ab = compareFn(a,b).signum
      val bc = compareFn(b,c).signum

      if( ab*bc >= 0 ) b
      else {
        val ac = compareFn(a,c)
        if( ac > 0 ) ab = -bc
        if( ab > 0 ) a else b
      }
    }
  }

  override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T,T) => Int, from: Int, until: Int ): Unit =
  {
    // create new every time -> deterministic behaviour
    val select = new SelectQuick( new PivotChooserV2 )
    select(values,i, compareFn, from,until)
  }
}

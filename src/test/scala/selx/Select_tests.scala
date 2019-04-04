/* This file is part of DelTri4S.
 *
 * DelTri4S is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DelTri4S is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DelTri4S.  If not, see <https://www.gnu.org/licenses/>.
 */

package selx

import utest._

import scala.util.Random

class Select_tests( select: Select ) extends TestSuite
{
  private val rng = new Random(1337)

  private def testDouble( compare: (Double,Double) => Int ) =
  {
    for( _ <- 1 to 64*1024 )
    {
      var arr = Array.tabulate[Double]( rng.nextInt(1024)+1 )( _ => rng.nextDouble )
      var ref = arr.clone

      val until = rng.nextInt(arr.length)+1
      val     i = rng.nextInt(until)
      val  from = rng.nextInt(i+1)

      select(arr,i, compare, from,until)
      var    j = 0
      while( j < from ) {
        assert( arr(j) == ref(j) )
        j += 1
      }
      while( j < i ) {
        assert( compare(arr(j),arr(i)) <= 0 )
        j += 1
      };j += 1
      while( j < until ) {
        assert( compare(arr(j),arr(i)) >= 0 )
        j += 1
      }
      while( j < arr.length ) {
        assert( arr(j) == ref(j) )
        j += 1
      }

      arr = arr.sortWith( compare(_,_) < 0 )
      ref = ref.sortWith( compare(_,_) < 0 )

      for( i <- 0 until arr.length )
        assert{ arr(i) == ref(i) }
    }
  }

  private def testInt( compare: (Int,Int) => Int ) =
  {
    for( _ <- 1 to 64*1024 )
    {
      val range = rng.nextInt(128)+1
      var arr = Array.tabulate[Int]( rng.nextInt(1024)+1 )( _ => rng.nextInt(range)+1 )
      var ref = arr.clone

      val until = rng.nextInt(arr.length)+1
      val     i = rng.nextInt(until)
      val  from = rng.nextInt(i+1)

      select(arr,i, compare, from,until)
      var    j = 0
      while( j < from ) {
        assert( arr(j) == ref(j) )
        j += 1
      }
      while( j < i ) {
        assert( compare(arr(j),arr(i)) <= 0 )
        j += 1
      };j += 1
      while( j < until ) {
        assert( compare(arr(j),arr(i)) >= 0 )
        j += 1
      }
      while( j < arr.length ) {
        assert( arr(j) == ref(j) )
        j += 1
      }

      arr = arr.sortWith( compare(_,_) < 0 )
      ref = ref.sortWith( compare(_,_) < 0 )

      for( i <- 0 until arr.length )
        assert{ arr(i) == ref(i) }
    }
  }

  private def testSmall( compare: (Int,Int) => Int ) = {
    def test( values: Array[Int] ) =
    {
      val ref = values sortWith {compare(_,_) < 0}

      for( from  <- 0   until values.length )
      for( until <- 1+from to values.length )
      for( i     <- from until until )
      {
        var arr = values.clone
        select(arr,i, compare, from,until)
        var    j = 0
        while( j < from ) {
          assert( arr(j) == values(j) )
          j += 1
        }
        while( j < i ) {
          assert( compare(arr(j),arr(i)) <= 0 )
          j += 1
        };j += 1
        while( j < until ) {
          assert( compare(arr(j),arr(i)) >= 0 )
          j += 1
        }
        while( j < arr.length ) {
          assert( arr(j) == values(j) )
          j += 1
        }

        arr = arr.sortWith( compare(_,_) < 0 )

        for( i <- 0 until arr.length )
          assert{ arr(i) == ref(i) }
      }
    }

    def inputs( len: Int ) = {
      var stream = Iterator(Nil: List[Int])
      for( _ <- 1 to len )
        stream = stream flatMap{ tail => 1 to len map (_ :: tail) }
      stream map (_.toArray)
    }

    for( len <- 1 to 6 )
    for( input <- inputs(len) )
      test(input)
  }

  override val tests = Tests{

    'smallAscending {
      testSmall{ (x,y) => (x-y).signum }
    }

    'smallDescending {
      testSmall{ (x,y) => (y-x).signum }
    }

    'doubleAscending {
      testDouble{ (x,y) => (x-y).signum }
    }

    'doubleDescending {
      testDouble{ (x,y) => (y-x).signum }
    }

    'intAscending {
      testInt{ (x,y) => (x-y).signum }
    }

    'intDescending {
      testInt{ (x,y) => (y-x).signum }
    }

  }
}

object Select_tests_MoM3V1   extends Select_tests( SelectMoM3v1  )
object Select_tests_MoM3V2   extends Select_tests( SelectMoM3v2  )
object Select_tests_MoM3V3   extends Select_tests( SelectMoM3v3  )
object Select_tests_MoM3V4   extends Select_tests( SelectMoM3v4  )

object Select_tests_MoM5V1   extends Select_tests( SelectMoM5v1  )
object Select_tests_MoM5V2   extends Select_tests( SelectMoM5v2  )
object Select_tests_MoM5V3   extends Select_tests( SelectMoM5v3  )
object Select_tests_MoM5V4   extends Select_tests( SelectMoM5v4  )
object Select_tests_MoM5V5   extends Select_tests( SelectMoM5v5  )
object Select_tests_MoM5V6   extends Select_tests( SelectMoM5v6  )
object Select_tests_MoM5V7   extends Select_tests( SelectMoM5v7  )

object Select_tests_MoMoM3V1 extends Select_tests( SelectMoMoM3v1  )
object Select_tests_MoMoM3V2 extends Select_tests( SelectMoMoM3v2  )
object Select_tests_MoMoM3V3 extends Select_tests( SelectMoMoM3v3  )
object Select_tests_MoMoM3V4 extends Select_tests( SelectMoMoM3v4  )

object Select_tests_QuickV1  extends Select_tests( SelectQuickV1 )
object Select_tests_QuickV2  extends Select_tests( SelectQuickV2 )

object Select_tests_Bubble   extends Select_tests( SelectBubble  )
object Select_tests_Heap     extends Select_tests( SelectHeap    )

object Select_tests_MeanV1   extends Select_tests(
  new  Select {
    override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T, T) => Int, from: Int, until: Int ): Unit
      = values.asInstanceOf[AnyRef] match {
          case _: Array[Double] =>
            val compare = compareFn.asInstanceOf[(Double,Double) => Int]
            val toDouble: T => Double
              = if( compare(-1,+1) < 0 ) x => +x.asInstanceOf[Double]
                else                     x => -x.asInstanceOf[Double]
            MeanSelectV1(values,toDouble,i, from,until)
          case _: Array[Int]    =>
            val compare = compareFn.asInstanceOf[(Int,Int) => Int]
            val toDouble: T => Double
              = if( compare(-1,+1) < 0 ) x => +1.0*x.asInstanceOf[Int]
                else                     x => -1.0*x.asInstanceOf[Int]
            MeanSelectV1(values,toDouble,i, from,until)
        }
  }
)

object Select_tests_MeanV2  extends Select_tests(
  new  Select {
    override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T, T) => Int, from: Int, until: Int ): Unit
      = values.asInstanceOf[AnyRef] match {
          case _: Array[Double] =>
            val compare = compareFn.asInstanceOf[(Double,Double) => Int]
            val toDouble: T => Double
              = if( compare(-1,+1) < 0 ) x => +x.asInstanceOf[Double]
                else                     x => -x.asInstanceOf[Double]
            MeanSelectV2(values,toDouble,i, from,until)
          case _: Array[Int]    =>
            val compare = compareFn.asInstanceOf[(Int,Int) => Int]
            val toDouble: T => Double
              = if( compare(-1,+1) < 0 ) x => +1.0*x.asInstanceOf[Int]
                else                     x => -1.0*x.asInstanceOf[Int]
            MeanSelectV2(values,toDouble,i, from,until)
        }
  }
)

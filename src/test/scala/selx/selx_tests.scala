package selx

import utest._
import java.util.Arrays

object selx_tests extends TestSuite
{
  override def tests = Tests{

    'median5v1 {
      var nCompare = 0L
      val  compareFn = (x: Int, y: Int) => {
          nCompare += 1
        (x-y).signum
      }

      for( a <- 1 to 5 )
      for( b <- 1 to 5 )
      for( c <- 1 to 5 )
      for( d <- 1 to 5 )
      for( e <- 1 to 5 ) {
        val ref = Array(a,b,c,d,e)
        val arr = ref.clone

        Median5v1(arr, compareFn, 0,2,3)

        assert( arr(0) <= arr(2) )
        assert( arr(1) <= arr(2) )
        assert(           arr(2) <= arr(3) )
        assert(           arr(2) <= arr(4) )
        Arrays sort ref
        Arrays sort arr
        assert{ Arrays equals (arr,ref) }
      }

      assert( nCompare <= 18750 ) // avoid regression
    }

    'median5v2 {
      var nCompare = 0L
      val  compareFn = (x: Int, y: Int) => {
          nCompare += 1
        (x-y).signum
      }

      for( a <- 1 to 5 )
      for( b <- 1 to 5 )
      for( c <- 1 to 5 )
      for( d <- 1 to 5 )
      for( e <- 1 to 5 ) {
        val ref = Array(a,b,c,d,e)
        val arr = ref.clone

        Median5v2(arr, compareFn, 0,2,3)

        assert( arr(0) <= arr(2) )
        assert( arr(1) <= arr(2) )
        assert(           arr(2) <= arr(3) )
        assert(           arr(2) <= arr(4) )
        Arrays sort ref
        Arrays sort arr
        assert{ Arrays equals (arr,ref) }
      }

      assert( nCompare <= 18996 ) // <- avoid regression
    }

    'median3 {
      var nCompare = 0L
      val  compareFn = (x: Int, y: Int) => {
        nCompare += 1
        (x-y).signum
      }

      for( a <- 1 to 5 )
      for( b <- 1 to 5 )
      for( c <- 1 to 5 ) {
        val ref = Array(a,b,c)
        val arr = ref.clone

        Median3(arr, compareFn, 0,1,2)

        Arrays sort ref
        assert{ Arrays equals (arr,ref) }
      }

      assert( nCompare <= 310 )
    }
  }
}

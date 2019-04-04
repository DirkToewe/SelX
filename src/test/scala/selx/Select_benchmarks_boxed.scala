package selx

import java.util.Arrays
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork( value = 4, jvmArgsAppend = Array("-Xmx8g") )
@Threads(1)
@Warmup(iterations = 16)
@Measurement(iterations = 8)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class Select_benchmarks_boxed
{
  @Param(Array(
    "10000000",
    "1000000",
    "100000",
    "10000",
    "1000",
    "100",
    "10",
    "1"
  ))
  var SIZE: Int = _

  final type DType = java.lang.Double

  private val rng = new Random(1337)
  private var array = null: Array[DType]
  private var k = -1
  private val compareFn = (x: DType, y: DType) => (x-y).signum
  private val id = (x: DType) => x*1.0
//  private val comparator: Comparator[JDouble] = (x,y) => compareFn(x,y)

  @Setup(Level.Trial) def init_array: Unit =
  {
    array = Array.fill(SIZE)(0.0)
  }

  @Setup(Level.Invocation) def init_k: Unit =
  {
    // always re-shuffle! Otherwise Arrays.sort has major advantage taking only O(n) on pre-sorted lists
    var     i = array.length
    while(  i > 0 ) {
            i-= 1
      array(i) = rng.nextDouble*2 - 1
    }
    k = rng nextInt array.length
  }

  @Benchmark
  def MoMoM3_v4(): Unit
    = SelectMoMoM3v4(array,k, compareFn)

  @Benchmark
  def MoM3_v3(): Unit
    = SelectMoM3v3(array,k, compareFn)

  @Benchmark
  def MoM5_v7(): Unit
    = SelectMoM5v7(array,k, compareFn)

  @Benchmark
  def Quick_v2(): Unit
    = SelectQuickV2(array,k, compareFn)

  @Benchmark
  def Mean_v2(): Unit
    = MeanSelectV2(array,id, k)

  @Benchmark
  def sort(): Unit
    = Arrays sort ( array, compareFn(_,_) )
}

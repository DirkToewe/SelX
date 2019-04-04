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

import java.awt.Desktop.getDesktop
import java.lang.Math.log
import java.lang.System.{arraycopy, nanoTime}
import java.nio.file.Files
import java.util.Arrays.asList
import java.util.{Arrays, Comparator}

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.regression.leastSquares

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Select_comparison
{
  def main( args: Array[String] ): Unit =
  {
//    type DType = java.lang.Double
    type DType = Double

    implicit object ord extends ( (DType,DType) => Int ) with Comparator[DType] {
      var comparisons = 0
      @inline override def apply( x: DType, y: DType ): Int = {
        comparisons += 1
        (x-y).signum
      }
      override def compare( x: DType, y: DType ): Int
        = this(x,y)
    }

    object SelectMeanV1 extends Select {
      override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T, T) => Int, from: Int, until: Int ): Unit
        = values.asInstanceOf[AnyRef] match {
            case _: Array[Double] => MeanSelectV1(values.asInstanceOf[Array[Double]], 1*(_: Double), i, from,until)
            case _                => MeanSelectV1(values, (x: T) => x.asInstanceOf[Double], i, from,until)
          }
    }

    object SelectMeanV2 extends Select {
      override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T, T) => Int, from: Int, until: Int ): Unit
        = values.asInstanceOf[AnyRef] match {
            case _: Array[Double] => MeanSelectV2(values.asInstanceOf[Array[Double]], 1*(_: Double), i, from,until)
            case _                => MeanSelectV2(values, (x: T) => x.asInstanceOf[Double], i, from,until)
          }
    }

    object SelectSort extends Select {
      override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T, T) => Int, from: Int, until: Int ): Unit
        = Arrays sort values.asInstanceOf[Array[Double]] ensuring 0 == from && until == values.length
    }

    object SelectSortBoxed extends Select {
      override def apply[@specialized T]( values: Array[T], i: Int, compareFn: (T, T) => Int, from: Int, until: Int ): Unit =
      {
        assert( 0 == from && until == values.length )
        val sorted = values sorted { compareFn(_,_) }
        arraycopy(sorted,0, values,0, values.length)
      }
    }

    val select_methods = SortedMap(
      "Mean v1"    -> SelectMeanV1,
      "Mean v2"    -> SelectMeanV2,
//      "MoM3 v1"    -> SelectMoM3V1,
//      "MoM3 v2"    -> SelectMoM3V2,
      "MoM3 v3"    -> SelectMoM3v3,
//      "MoM3 v4"    -> SelectMoM3V4,
//      "MoM5 v1"   -> SelectMoM5V1,
//      "MoM5 v2"   -> SelectMoM5V2,
//      "MoM5 v3"   -> SelectMoM5V3,
//      "MoM5 v4"   -> SelectMoM5V4,
//      "MoM5 v5"   -> SelectMoM5V5,
//      "MoM5 v6"   -> SelectMoM5V6,
      "MoM5 v7"   -> SelectMoM5v7,
//      "Heap"      -> SelectHeap,
//      "MoMoM3 v1" -> SelectMoMoM3V1,
//      "MoMoM3 v2" -> SelectMoMoM3V2,
//      "MoMoM3 v3" -> SelectMoMoM3V3,
      "MoMoM3 v4" -> SelectMoMoM3v4,
//      "Quick v1"  -> SelectQuickV1,
      "Quick v2"  -> SelectQuickV2,
      "Sort"      -> SelectSort,
      "SortBoxed" -> SelectSortBoxed,
//      "Bubble"    -> SelectBubble
    )

    val sizes = ArrayBuffer.empty[Int]
    val times       = select_methods map {_._1 -> ArrayBuffer.empty[Double]}
    val comparisons = select_methods map {_._1 -> ArrayBuffer.empty[Long]  }

    val rng = new Random()
    for( run <- 1 to 1024 )
    {
//      assert(size > 0)
//      val VALUES = Array.fill[Double]( rng.nextInt(128)+1 )(rng.nextDouble)
      val VALUES = Array.fill[DType]( rng.nextInt(1024*1024)+1 )(rng.nextDouble)
//      val VALUES = Array.fill( rng.nextInt(1024)+1 )(rng.nextDouble)
//      val VALUES = Array.fill( rng.nextInt(1024*1024)+1 )( if(rng.nextBoolean) 1.0 else 0.0 )

//      val i = rng nextInt VALUES.length
      val i = VALUES.length/2

//      System.gc()

      sizes += VALUES.length

      val         SORTED = VALUES.clone
      Arrays sort SORTED
//      Arrays sort (SORTED,ord)

      for( (name,select) <- rng.shuffle(select_methods) )
      {
//        System.gc()

        ord.comparisons = 0

        val values = VALUES.clone

        val t0 = nanoTime
        select(values, i, ord)
        val dt = nanoTime - t0

        times      (name) += dt
        comparisons(name) += ord.comparisons

        for( j <- 0 to i                ) assert( values(j) <= values(i) )
        for( j <- i until values.length ) assert( values(j) >= values(i) )

        Arrays.sort(values)
//        Arrays sort (values,ord)

        for( i <- 0 until SORTED.length )
          assert( SORTED(i) == values(i) )
      }

      if( run % 8 == 0 )
        println(f"Run${run}%7d check!")
    }

    println("\nTIMES:")
    for( (method,time) <- times.mapValues(_.sum).toArray.sortBy(_._2) ) {
      printf("%9s:%7.3f\n", method, time / 1e9)
    }
    println("\nCOMPARISONS:")
    for( (method,nCompare) <- comparisons.mapValues(_.sum).toArray.sortBy(_._2) ) {
      printf("%9s:%10d\n", method, nCompare)
    }

    def plot( title: String, data: Iterable[String] ) =
    {
      val plot = s"""
       |<!DOCTYPE html>
       |<html lang=\"en\">
       |  <head>
       |    <meta charset=\"utf-8\">
       |    <title>Select Algorithm $title</title>
       |    <script type=\"text/javascript\" src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>
       |  </head>
       |  <body>
       |    <script type=\"text/javascript\">
       |    'use strict'; {
       |      let div = document.createElement('div');
       |      div.style = 'width: 100%; height: 1024px';
       |      div.innerHTML = 'Creating Plot...';
       |      document.body.appendChild(div);
       |
       |      const size = [${sizes mkString ", "}];
       |      const sizeSorted = [...size].sort( (x,y) => x-y );
       |
       |      let
       |        data = [
       |          ${data mkString ",\n"}
       |        ],
       |        layout = { title: 'Select Algorithm $title' };
       |      div.innerHTML = '';
       |      Plotly.plot(div, data, layout, { showLink: false, modeBarButtonsToRemove: ['sendDataToCloud'] });
       |    }
       |    </script>
       |  </body>
       |</html>
      """.stripMargin

      val tmp = Files.createTempFile("plot_",".html")
      Files.write(tmp, asList(plot) )
      printf("%s written to %s\n", title, tmp)
//      getDesktop.browse(tmp.toUri)
    }

    def fit[N <% Double]( y: Seq[N] ): String = {
      val c = leastSquares(
        DenseMatrix[Array[Double],Double](
          sizes.map( x => Array( x, x*log(x) ) ) :_*
        ),
        DenseVector{ y.map(_*1.0): _* }
      ).coefficients

      s"(x => ${c(0)}*x + ${c(1)}*x*Math.log(x))"
    }

    val sizesCurve = sizes.sorted

    val colors: Iterator[String] = {
      // http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=10
      val colors = Array(
        "#e41a1c",
        "#377eb8",
        "#4daf4a",
        "#984ea3",
        "#ff7f00",
        "#ffff33",
        "#a65628",
        "#f781bf",
        "#999999"
      )
      assert(colors.length >= select_methods.size)
      Iterator.continually( rng shuffle colors.take(select_methods.size).toIndexedSeq ).take(2).flatten
    }

    plot(
      "Timings",
      times flatMap {
        case (name,times) =>
          val f = fit(times)
          val col = colors.next()
          Seq(
            s"""
             |{
             |  type: 'scattergl', mode: 'markers', name: '$name',
             |  marker: { color: '$col' },
             |  x: size,
             |  y: [${times map { x => f"$x%.3f" } mkString ", "}]
             |}
            """.stripMargin,
            s"""
             |{
             |  type: 'scattergl', mode: 'lines', name: '$name (fit)',
             |  line: { color: '$col' },
             |  x: sizeSorted,
             |  y: sizeSorted.map($f)
             |}
            """.stripMargin
          )
      }
    )

    plot(
      "Comparisons",
      comparisons flatMap {
        case (name,comparisons) =>
          val f = fit(comparisons)
          val col = colors.next()
          Seq(
           s"""
             |{
             |  type: 'scattergl', mode: 'markers', name: '$name',
             |  marker: { color: '$col' },
             |  x: size,
             |  y: [${comparisons map { x => f"$x" } mkString ", "}]
             |}
            """.stripMargin,
            s"""
             |{
             |  type: 'scattergl', mode: 'lines', name: '$name (fit)',
             |  line: { color: '$col' },
             |  x: sizeSorted,
             |  y: sizeSorted.map($f)
             |}
            """.stripMargin
          )
      }
    )
  }
}

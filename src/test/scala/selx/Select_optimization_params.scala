package selx

import java.lang.{Double => JDouble}
import java.nio.file.Files
import java.util.Arrays.asList

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Parameter optimization of `SelectHeap` with respect to minimum number of comparisons.
  */
object Select_optimization_params
{
  def main( args: Array[String] ): Unit =
  {
    val nRepeat = 128
    val plotData = ArrayBuffer.empty[String]

    for( size <- Iterator.iterate(7*7*7)(_*7) take 5 )
    {
      println(size)

      val x = Array.range(128, 128*3+1, 8).reverse
      val y = ArrayBuffer.empty[Double]

      for( n <- x )
      {
        printf("  %3d\n", n)
//        MeanSelectV2.N = n

        val nCompare: Double = (1 to nRepeat).par.map{ _ =>
          val array = Array.tabulate[JDouble](size){
            val  rng = new Random
            _ => rng.nextDouble()*2 - 1
          }

          object compareFn extends ((JDouble,JDouble) => Int) {
            var nCompare = 0L
            override def apply( x: JDouble, y: JDouble ) = {
              nCompare += 1
              (x-y).signum
            }
          }

//          SelectMoMoM3V3(array,array.length >>> 1, compareFn)
          compareFn.nCompare
        }.sum

        y += nCompare / nRepeat / size
      }

      plotData += s"""
       | {
       |   type: 'scattergl',
       |   mode: 'lines',
       |   name: 'size = $size',
       |   x: [${ x mkString ", " }],
       |   y: [${ y mkString ", " }]
       | }
      """.stripMargin
    }

    val plot = s"""
     |<!DOCTYPE html>
     |<html lang=\"en\">
     |  <head>
     |    <meta charset=\"utf-8\">
     |    <title>SelectMoM5V4 - Parameter Optimization</title>
     |    <script type=\"text/javascript\" src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>
     |  </head>
     |  <body>
     |    <script type=\"text/javascript\">
     |    'use strict'; {
     |      let div = document.createElement('div');
     |      div.style = 'width: 100%; height: 1024px';
     |      div.innerHTML = 'Creating Plot...';
     |      document.body.appendChild(div);
     |      let
     |        data = [
     |          ${plotData mkString ",\n"}
     |        ],
     |        layout = { title: 'SelectMoM5V4 - Parameter Optimization' };
     |      div.innerHTML = '';
     |      Plotly.plot(div, data, layout, { showLink: false, modeBarButtonsToRemove: ['sendDataToCloud'] });
     |    }
     |    </script>
     |  </body>
     |</html>
    """.stripMargin

    val tmp = Files.createTempFile("plot_",".html")
    Files.write(tmp, asList(plot) )
  }
}

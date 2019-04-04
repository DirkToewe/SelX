SelX is an exploratory project that compares the performance and efficiency of different
[Selection Algorithms](https://en.wikipedia.org/wiki/Selection_algorithm). As a baseline
the algorithms are also compared to `java.util.Arrays.sort`. SelX implements multiple
evolutions of the following Selection Algorithms:

<dl>
<dt>Bubble Select<dd>
  Inspired by <a href="https://en.wikipedia.org/wiki/Bubble_sort">Bubble Sort</a>. Moves the maxima
  from the left side to the right until the Selection property is instated.
<dt>Heap Select<dd>
  Builds a <a href="https://en.wikipedia.org/wiki/Binary_heap">Binary Heap</a> on the larger
  of the two sides and swaps with the other side until the Selection property is instated.
<dt>Median-of-Medians (MoM)<dd>
  Splits the input into small groups of 3 (MoM3) or 5 (MoM5) entries, computes the median
  of each groups. Of these medians, the median is computed by calling MoM recursively. This
  median is then used to split/pivotize the original input into two parts. The entire procedure
  is repeated on the correct one of the two parts recursively. MoM5 is guaranteed O(n).
<dt>Median-of-Medians-of-Medians (MoMoM3)<dd>
  Also known as repeated step algorithm. Splits the input into groups of 3 and computes their
  median. Those medians are again split into groups of 3, their median is computed. Of those
  medians, the median is computed by recursively calling MoMoM3. That one resulting median
  is then used to split/pivotize the original input into two parts. The entire procedure
  is repeated on the correct one of the two parts recursively. MoMoM3 is guaranteed
  O(n) and slightly faster than MoM5.
<dt>Quick Select<dd>
  Chooses a random entry from the input and uses it to split/pivotize the input. This is done
  recursively until the Selection property is instated.
<dt>Mean Select<dd>
  Like Quick Select but uses the mean value of th inputs to split/pivotize. (Requires numeric keys).
</dl>

Most of the algorithms have been incrementally tweaked and optimized. The most effectful
optimizations were taken from [this paper](https://arxiv.org/abs/1606.00484):
  * Use the median of three random values as pivot for Quick Select
  * Avoid splitting/pivotizing the medians of the groups a second time
  * Instead of just selecting the median of the medians, select another index of the medians
    if it guarantees a better reduction of the problem size while splitting/pivotizing.

Running the Benchmarks
----------------------
To run a quick benchmark that will automatically generate HTML plots of the results, use sbt to call:
```
test:runMain test:runMain selx.Select_comparison
```

To run the proper [JMH](https://openjdk.java.net/projects/code-tools/jmh/) benchmarks, which may take roughly 24 hours, run:
```
jmh:run
```

To run only the unboxed or boxed JMH benchmark, run:
```
jmh:run selx.Select_benchmarks_double
```
or:
```
jmh:run selx.Select_benchmarks_boxed
```

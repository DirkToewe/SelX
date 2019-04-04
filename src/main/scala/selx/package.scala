package object selx
{
  @inline private[selx] def swap[@specialized T](values: Array[T], i: Int, j: Int ): Unit =
  {
    val tmp = values(i)
              values(i) = values(j)
                          values(j) = tmp
  }
}

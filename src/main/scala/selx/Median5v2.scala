package selx

/** Selects the middle of 5 entries in an array.
  */
private[selx] object Median5v2
{
  @inline private[selx] def apply[@specialized T](values: Array[T], compareFn: (T,T) => Int, l: Int, i: Int, r: Int ): Unit =
  {
    // takes 4-8 comparisons
    @inline def <( i: Int, j: Int ) = compareFn( values(i), values(j) ) < 0
    @inline def a = l+0
    @inline def b = l+1
    @inline def c = i
    @inline def d = r+0
    @inline def e = r+1

    if( <(b,a) ) swap(values,b,a)
    if( <(e,d) ) swap(values,e,d)

    if(       <(c,b) ) { swap(values,c,b)
      if(     <(d,c) ) { swap(values,d,c)
        if(   <(c,b) ) { swap(values,c,b)
          if( <(c,a) )   swap(values,c,a)
          if( <(e,c) ) { swap(values,e,c)
          if( <(c,a) )   swap(values,c,a) }
        }
        else if( <(c,a) ) { swap(values,c,a)
             if( <(e,c) )   swap(values,e,c) }}}
    else if( <(d,c) ) { swap(values,d,c)
         if( <(c,b) ) { swap(values,c,b)
         if( <(e,c) ) { swap(values,e,c)
         if( <(c,a) )   swap(values,c,a) }}}
  }
}

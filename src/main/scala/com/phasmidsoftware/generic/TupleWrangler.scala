/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.generic

/**
  * A collection of methods to manipulate tuples.
  */
object TupleWrangler:

  /**
    * Method to unpack a nested tuple of form ((A, B), C) into a non-nested tuple: (A, B, C).
    * This is particularly useful if zipping for example xs zip ys zip zs.
    *
    * @param t a nested 2-tuple of form ((A, B), C).
    * @tparam A the underlying type of the first element.
    * @tparam B the underlying type of the first element.
    * @tparam C the underlying type of the first element.
    * @return a 3-tuple of form (A, B, C).
    */
  def inline[A, B, C](t: ((A, B), C)): (A, B, C) = (t._1._1, t._1._2, t._2)


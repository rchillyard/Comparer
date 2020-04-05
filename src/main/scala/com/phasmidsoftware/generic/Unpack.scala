package com.phasmidsoftware.generic

object Unpack {

  /**
    * Method to unpack a nested tuple of form ((A, B), C) into a non-nested tuple: (A, B, C).
    *
    * @param t a nested 2-tuple of form ((A, B), C).
    * @tparam A the underlying type of the first element.
    * @tparam B the underlying type of the first element.
    * @tparam C the underlying type of the first element.
    * @return a 3-tuple of form (A, B, C).
    */
  def unpack[A, B, C](t: ((A, B), C)): (A, B, C) = (t._1._1, t._1._2, t._2)

}

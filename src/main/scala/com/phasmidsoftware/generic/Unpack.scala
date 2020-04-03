package com.phasmidsoftware.generic

object Unpack {

  def unpack[A, B, C](t: ((A, B), C)): (A, B, C) = (t._1._1, t._1._2, t._2)

}

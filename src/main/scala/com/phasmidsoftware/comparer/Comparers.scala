/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer


/**
  * Definitions of the various comparers for different underlying types.
  *
  */
trait Comparers {

  /**
    * Method to return a Comparer[Option[T] where T is any type that has an implicit Comparer.
    * Note that we arbitrarily evaluate all comparisons involving None to be Same.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Option[T] which can compare two instances of Option[T] and returns a Comparison.
    */
  implicit def comparerOpt[T: Comparer]: Comparer[Option[T]] = to1 => to2 =>
    to1 match {
      case Some(t1) => to2 match {
        case Some(t2) => implicitly[Comparer[T]].apply(t1)(t2)
        case None => Same
      }
      case None => Same
    }

  /**
    * Method to return a Comparer[Seq[T] where T is any type that has an implicit Comparer.
    * Elements at the head of a sequence are considered more significant than those appearing later;
    * the comparisons are made only through the shortest sequence.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Seq[T] which can compare two instances of Seq[T] and returns a Comparison.
    */
  implicit def comparerSeq[T: Comparer]: Comparer[Seq[T]] = to2 => to1 =>
    (to1 zip to2).foldLeft[Comparison](Same)((a, x) => a orElse Comparison.compare(x._1, x._2))

  /**
    * Method to return a Comparer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
    *
    * NOTE: be careful using this method it only applies where T is a 1-tuple (e.g. a case class with one field).
    *
    * @param f a function P => T, usually the apply method of a case class.
    * @tparam P the type of the (single) field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer1[P: Comparer, T <: Product](f: P => T): Comparer[T] = comparer[T, P](0)

  /**
    * Method to return a Comparer[T] where T is a 2-ary Product and which is based on a function to convert a P0,P1 into a T.
    *
    * @param f a function (P0,P1) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer2[P0: Comparer, P1: Comparer, T <: Product](f: (P0, P1) => T): Comparer[T] =
    comparer[T, P0](0) orElse
      comparer[T, P1](1)

  /**
    * Method to return a Comparer[T] where T is a 3-ary Product and which is based on a function to convert a P0,P1,P2 into a T.
    *
    * @param f a function (P0,P1,P2) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer3[P0: Comparer, P1: Comparer, P2: Comparer, T <: Product](f: (P0, P1, P2) => T): Comparer[T] =
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2)

  /**
    * Method to return a Comparer[T] where T is a 4-ary Product and which is based on a function to convert a P0,P1,P2,P3 into a T.
    *
    * @param f a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer4[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, T <: Product](f: (P0, P1, P2, P3) => T): Comparer[T] =
    Comparer.comparer[T, P0](t => t.productElement(0).asInstanceOf[P0]) orElse
      Comparer.comparer(t => t.productElement(1).asInstanceOf[P1]) orElse
      Comparer.comparer(t => t.productElement(2).asInstanceOf[P2]) orElse
      Comparer.comparer(t => t.productElement(3).asInstanceOf[P3])

  /**
    * Method to return a Comparer[T] where T is a 5-ary Product and which is based on a function to convert a P0,P1,P2,P3,P4 into a T.
    *
    * @param f a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam P4 the type of the fifth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer5[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, P4: Comparer, T <: Product](f: (P0, P1, P2, P3, P4) => T): Comparer[T] =
    Comparer.comparer[T, P0](t => t.productElement(0).asInstanceOf[P0]) orElse
      Comparer.comparer(t => t.productElement(1).asInstanceOf[P1]) orElse
      Comparer.comparer(t => t.productElement(2).asInstanceOf[P2]) orElse
      Comparer.comparer(t => t.productElement(3).asInstanceOf[P3]) orElse
      Comparer.comparer(t => t.productElement(4).asInstanceOf[P4])

  /**
    * Method to return a Comparer[T] where T is a 6-ary Product and which is based on a function to convert a P0,P1,P2,P3,P4,P5 into a T.
    *
    * @param f a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam P4 the type of the fifth field of the Product type T.
    * @tparam P5 the type of the sixth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer6[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, P4: Comparer, P5: Comparer, T <: Product](f: (P0, P1, P2, P3, P4, P5) => T): Comparer[T] =
    Comparer.comparer[T, P0](t => t.productElement(0).asInstanceOf[P0]) orElse
      Comparer.comparer(t => t.productElement(1).asInstanceOf[P1]) orElse
      Comparer.comparer(t => t.productElement(2).asInstanceOf[P2]) orElse
      Comparer.comparer(t => t.productElement(3).asInstanceOf[P3]) orElse
      Comparer.comparer(t => t.productElement(4).asInstanceOf[P4]) orElse
      Comparer.comparer(t => t.productElement(5).asInstanceOf[P5])

  // CONSIDER add more comparer methods, including one for Try, etc.

  private def comparer[T <: Product, P: Comparer](x: Int): Comparer[T] = Comparer.comparer[T, P](t => t.productElement(x).asInstanceOf[P])
}

/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer


/**
  * Trait to define the various comparers for different underlying types.
  *
  */
trait Comparers {

  /**
    * Method to return a Comparer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
    *
    * NOTE: be careful using this method it only applies where T is a 1-tuple (e.g. a case class with one field).
    *
    * @param construct a function P => T, usually the apply method of a case class.
    * @tparam P0 the type of the (single) field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer1[P0: Comparer, T <: Product](construct: P0 => T): Comparer[T] =
    Comparer.comparer(_.productElement(0).asInstanceOf[P0])

  /**
    * Method to return a Comparer[T] where T is a 2-ary Product and which is based on a function to convert a P0,P1 into a T.
    *
    * @param construct a function (P0,P1) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer2[P0: Comparer, P1: Comparer, T <: Product](construct: (P0, P1) => T): Comparer[T] =
    Comparer.comparer[T, P0](t => t.productElement(0).asInstanceOf[P0]) orElse
      Comparer.comparer(t => t.productElement(1).asInstanceOf[P1])

  /**
    * Method to return a Comparer[T] where T is a 3-ary Product and which is based on a function to convert a P0,P1,P2 into a T.
    *
    * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer3[P0: Comparer, P1: Comparer, P2: Comparer, T <: Product](construct: (P0, P1, P2) => T): Comparer[T] =
    Comparer.comparer[T, P0](t => t.productElement(0).asInstanceOf[P0]) orElse
      Comparer.comparer(t => t.productElement(1).asInstanceOf[P1]) orElse
      Comparer.comparer(t => t.productElement(2).asInstanceOf[P2])

  /**
    * Method to return a Comparer[T] where T is a 4-ary Product and which is based on a function to convert a P0,P1,P2,P3 into a T.
    *
    * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the third field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer4[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, T <: Product](construct: (P0, P1, P2, P3) => T): Comparer[T] =
    Comparer.comparer[T, P0](t => t.productElement(0).asInstanceOf[P0]) orElse
      Comparer.comparer(t => t.productElement(1).asInstanceOf[P1]) orElse
      Comparer.comparer(t => t.productElement(2).asInstanceOf[P2]) orElse
      Comparer.comparer(t => t.productElement(3).asInstanceOf[P3])

  /**
    * Method to return a Comparer[T] where T is a 5-ary Product and which is based on a function to convert a P0,P1,P2,P3,P4 into a T.
    *
    * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam P4 the type of the fifth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer5[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, P4: Comparer, T <: Product](construct: (P0, P1, P2, P3, P4) => T): Comparer[T] =
    Comparer.comparer[T, P0](t => t.productElement(0).asInstanceOf[P0]) orElse
      Comparer.comparer(t => t.productElement(1).asInstanceOf[P1]) orElse
      Comparer.comparer(t => t.productElement(2).asInstanceOf[P2]) orElse
      Comparer.comparer(t => t.productElement(3).asInstanceOf[P3]) orElse
      Comparer.comparer(t => t.productElement(4).asInstanceOf[P4])

  // TODO add more comparer methods, including Option, Try, Seq, etc.
}

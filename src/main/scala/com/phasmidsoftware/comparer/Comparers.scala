/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import scala.util.Try


/**
  * Definitions of the various comparers for different underlying types.
  *
  */
trait Comparers {

  /**
    * Method to return a Comparer[Iterable[T] where T is any type that has an implicit Comparer.
    * Elements at the head of a iterable are considered more significant than those appearing later;
    * Comparisons are made only through the shortest iterable.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Iterable[T] which can compare two instances of Iterable[T] and return a Comparison.
    */
  implicit def comparerIterable[T: Comparer]: Comparer[Iterable[T]] = to2 => to1 =>
    (to1 zip to2).foldLeft[Comparison](Same)((a, x) => a orElse Comparison.compare(x._1, x._2))

  /**
    * Method to return a Comparer[Iterable[T] where T is any type that has an implicit Comparer.
    * Elements at the head of a iterable are considered more significant than those appearing later;
    * Comparisons are made only through the shortest iterable.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Iterable[T] which can compare two instances of Iterable[T] and return a Comparison.
    */
  implicit def comparerSeq[T: Comparer]: Comparer[Seq[T]] = {
    // NOTE: this construction is necessary to avoid diverging implicit expansion compiler error: don't inline.
    val comparer: Comparer[Iterable[T]] = comparerIterable
    comparer.snap(identity)
  }

  /**
    * Method to return a Comparer[List[T] where T is any type that has an implicit Comparer.
    * Elements at the head of a list are considered more significant than those appearing later;
    * Comparisons are made only through the shortest list.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of List[T] which can compare two instances of List[T] and return a Comparison.
    */
  implicit def comparerList[T: Comparer]: Comparer[List[T]] = {
    // NOTE: this construction is necessary to avoid diverging implicit expansion compiler error: don't inline.
    val comparer: Comparer[Iterable[T]] = comparerIterable
    comparer.snap(identity)
  }

  /**
    * Method to return a Comparer[Array[T] where T is any type that has an implicit Comparer.
    * Elements at the head of a array are considered more significant than those appearing later;
    * Comparisons are made only through the shortest array.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Array[T] which can compare two instances of Array[T] and return a Comparison.
    */
  implicit def comparerArray[T: Comparer]: Comparer[Array[T]] = to2 => to1 =>
    (to1 zip to2).foldLeft[Comparison](Same)((a, x) => a orElse Comparison.compare(x._1, x._2))

  /**
    * Method to return a Comparer[Option[T] where T is any type that has an implicit Comparer.
    * Note that we arbitrarily evaluate all comparisons involving None to be Same.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Option[T] which can compare two instances of Option[T] and return a Comparison.
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
    * Method to return a Comparer[Try[T] where T is any type that has an implicit Comparer.
    * Note that we arbitrarily evaluate all comparisons involving Failure to be Same.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Try[T] which can compare two instances of Option[T] and return a Comparison.
    */
  implicit def comparerTry[T: Comparer]: Comparer[Try[T]] = {
    // NOTE: this construction is necessary to avoid diverging implicit expansion compiler error: don't inline.
    val toc: Comparer[Option[T]] = comparerOpt
    toc.snap(_.toOption)
  }

  /**
    * Method to return a Comparer[Either[L,R] where R is any type that has an implicit Comparer.
    * Note that we arbitrarily evaluate all comparisons involving L to be Same.
    *
    * @tparam L the underlying left-hand type of the inputs.
    * @tparam R the underlying right-hand type of the inputs (must have a Comparer defined).
    * @return a Comparer of Either[L,R] which can compare two instances of Either[L,R] and return a Comparison.
    */
  implicit def comparerEither[L, R: Comparer]: Comparer[Either[L, R]] = {
    // NOTE: this construction is necessary to avoid diverging implicit expansion compiler error: don't inline.
    val toc: Comparer[Option[R]] = comparerOpt
    toc.snap(_.toOption)
  }

  /**
    * Method to return a Comparer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
    *
    * NOTE: be careful using this method it only applies where T is a 1-tuple (e.g. a case class with one field).
    *
    * @param f a function P => T, usually the apply method of a case class.
    * @tparam P the type of the (single) field of the Product type T.
    * @tparam T the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer1[P: Comparer, T <: Product](f: P => T): Comparer[T] = comparer[T, P](0)

  /**
    * Method to return a Comparer[T] where T is a 2-ary Product and which is based on a function to convert a (P0,P1) into a T.
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
    * Method to return a Comparer[T] where T is a 3-ary Product and which is based on a function to convert a (P0,P1,P2) into a T.
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
    * Method to return a Comparer[T] where T is a 4-ary Product and which is based on a function to convert a (P0,P1,P2,P3) into a T.
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
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2) orElse
      comparer[T, P3](3)

  /**
    * Method to return a Comparer[T] where T is a 5-ary Product and which is based on a function to convert a (P0,P1,P2,P3,P4) into a T.
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
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2) orElse
      comparer[T, P3](3) orElse
      comparer[T, P4](4)

  /**
    * Method to return a Comparer[T] where T is a 6-ary Product and which is based on a function to convert a (P0,P1,P2,P3,P4,P5) into a T.
    *
    * @param f a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
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
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2) orElse
      comparer[T, P3](3) orElse
      comparer[T, P4](4) orElse
      comparer[T, P5](5)

  /**
    * Method to return a Comparer[T] where T is a 7-ary Product and which is based on a function to convert a (P0,P1,P2,P3,P4,P5,P6) into a T.
    *
    * @param f a function (P0,P1,P2,P3,P4,P5,P6) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam P4 the type of the fifth field of the Product type T.
    * @tparam P5 the type of the sixth field of the Product type T.
    * @tparam P6 the type of the seventh field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer7[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, P4: Comparer, P5: Comparer, P6: Comparer, T <: Product](f: (P0, P1, P2, P3, P4, P5, P6) => T): Comparer[T] =
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2) orElse
      comparer[T, P3](3) orElse
      comparer[T, P4](4) orElse
      comparer[T, P5](5) orElse
      comparer[T, P6](6)

  /**
    * Method to return a Comparer[T] where T is a 8-ary Product and which is based on a function to convert a (P0,P1,P2,P3,P4,P5,P6,P7) into a T.
    *
    * @param f a function (P0,P1,P2,P3,P4,P5,P6,P7) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam P4 the type of the fifth field of the Product type T.
    * @tparam P5 the type of the sixth field of the Product type T.
    * @tparam P6 the type of the seventh field of the Product type T.
    * @tparam P7 the type of the eighth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer8[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, P4: Comparer, P5: Comparer, P6: Comparer, P7: Comparer, T <: Product](f: (P0, P1, P2, P3, P4, P5, P6, P7) => T): Comparer[T] =
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2) orElse
      comparer[T, P3](3) orElse
      comparer[T, P4](4) orElse
      comparer[T, P5](5) orElse
      comparer[T, P6](6) orElse
      comparer[T, P7](7)

  /**
    * Method to return a Comparer[T] where T is a 9-ary Product and which is based on a function to convert a (P0,P1,P2,P3,P4,P5,P6,P7,P8) into a T.
    *
    * @param f a function (P0,P1,P2,P3,P4,P5,P6,P7,P8) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam P4 the type of the fifth field of the Product type T.
    * @tparam P5 the type of the sixth field of the Product type T.
    * @tparam P6 the type of the seventh field of the Product type T.
    * @tparam P7 the type of the eighth field of the Product type T.
    * @tparam P8 the type of the ninth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer9[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, P4: Comparer, P5: Comparer, P6: Comparer, P7: Comparer, P8: Comparer, T <: Product](f: (P0, P1, P2, P3, P4, P5, P6, P7, P8) => T): Comparer[T] =
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2) orElse
      comparer[T, P3](3) orElse
      comparer[T, P4](4) orElse
      comparer[T, P5](5) orElse
      comparer[T, P6](6) orElse
      comparer[T, P7](7) orElse
      comparer[T, P8](8)

  /**
    * Method to return a Comparer[T] where T is a 10-ary Product and which is based on a function to convert a (P0,P1,P2,P3,P4,P5,P6,P7,P8,P9) into a T.
    *
    * @param f a function (P0,P1,P2,P3,P4,P5,P6,P7,P8,P9) => T, usually the apply method of a case class.
    * @tparam P0 the type of the first field of the Product type T.
    * @tparam P1 the type of the second field of the Product type T.
    * @tparam P2 the type of the third field of the Product type T.
    * @tparam P3 the type of the fourth field of the Product type T.
    * @tparam P4 the type of the fifth field of the Product type T.
    * @tparam P5 the type of the sixth field of the Product type T.
    * @tparam P6 the type of the seventh field of the Product type T.
    * @tparam P7 the type of the eighth field of the Product type T.
    * @tparam P8 the type of the ninth field of the Product type T.
    * @tparam P9 the type of the tenth field of the Product type T.
    * @tparam T  the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer10[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, P4: Comparer, P5: Comparer, P6: Comparer, P7: Comparer, P8: Comparer, P9: Comparer, T <: Product](f: (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): Comparer[T] =
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2) orElse
      comparer[T, P3](3) orElse
      comparer[T, P4](4) orElse
      comparer[T, P5](5) orElse
      comparer[T, P6](6) orElse
      comparer[T, P7](7) orElse
      comparer[T, P8](8) orElse
      comparer[T, P9](9)

  /**
    * Method to return a Comparer[T] where T is a 10-ary Product and which is based on a function to convert a (P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) into a T.
    *
    * @param f a function (P0,P1,P2,P3,P4,P5,P6,P7,P8,P9) => T, usually the apply method of a case class.
    * @tparam P0  the type of the first field of the Product type T.
    * @tparam P1  the type of the second field of the Product type T.
    * @tparam P2  the type of the third field of the Product type T.
    * @tparam P3  the type of the fourth field of the Product type T.
    * @tparam P4  the type of the fifth field of the Product type T.
    * @tparam P5  the type of the sixth field of the Product type T.
    * @tparam P6  the type of the seventh field of the Product type T.
    * @tparam P7  the type of the eighth field of the Product type T.
    * @tparam P8  the type of the ninth field of the Product type T.
    * @tparam P9  the type of the tenth field of the Product type T.
    * @tparam P10 the type of the eleventh field of the Product type T.
    * @tparam T   the underlying type of the result, a Product.
    * @return a Comparer[T] which can compare two instances of T and return a Comparison.
    */
  def comparer11[P0: Comparer, P1: Comparer, P2: Comparer, P3: Comparer, P4: Comparer, P5: Comparer, P6: Comparer, P7: Comparer, P8: Comparer, P9: Comparer, P10: Comparer, T <: Product](f: (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): Comparer[T] =
    comparer[T, P0](0) orElse
      comparer[T, P1](1) orElse
      comparer[T, P2](2) orElse
      comparer[T, P3](3) orElse
      comparer[T, P4](4) orElse
      comparer[T, P5](5) orElse
      comparer[T, P6](6) orElse
      comparer[T, P7](7) orElse
      comparer[T, P8](8) orElse
      comparer[T, P9](9) orElse
      comparer[T, P10](10)

  private def comparer[T <: Product, P: Comparer](x: Int): Comparer[T] = Comparer.comparer[T, P](t => t.productElement(x).asInstanceOf[P])
}

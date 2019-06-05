/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import scala.language.{implicitConversions, postfixOps}

/**
  * Type class trait Comparer[T].
  *
  * The behavior of this trait is that it is a lazy comparer of two T instances, presented in the form of a tuple.
  * The result of the comparison is a Comparison object.
  *
  * @tparam T the underlying type of the comparer. That's to say the type this Comparer knows about.
  */
trait Comparer[T] extends (((T, T)) => Comparison) {
  self =>

  /**
    * Method to convert this Comparer[T] into an Ordering[T] which can then be used for more typical Java/Scala-style comparisons.
    *
    * @return a new Ordering[T].
    */
  //noinspection ConvertExpressionToSAM
  def toOrdering: Ordering[T] = new Ordering[T]() {
    def compare(x: T, y: T): Int = self(x, y).toInt
  }

  /**
    * Method to yield a Boolean from this Comparer, given a tuple of two Ts.
    *
    * @param tt the tuple of Ts. Try saying that a few times!
    * @return a Boolean which is true if the left T is greater than the right T.
    */
  def >(tt: (T, T)): Boolean = self(tt).flip().getOrElse(false)

  /**
    * Method to yield a Boolean from this Comparer, given a tuple of two Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the left T is less than the right T.
    */
  def <(tt: (T, T)): Boolean = self(tt)().getOrElse(false)

  /**
    * Method to yield a Boolean from this Comparer, given a tuple of two Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the Ts compare as the same.
    */
  def ==(tt: (T, T)): Boolean = self(tt)().isEmpty

  /**
    * Method to yield a Boolean from this Comparer, given a tuple of two Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the left T is greater than or equal to the right T.
    */
  def >=(tt: (T, T)): Boolean = ! <(tt)

  /**
    * Method to yield a Boolean from this Comparer, given a tuple of two Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the left T is less than or equal to the right T.
    */
  def <=(tt: (T, T)): Boolean = ! >(tt)

  /**
    * Method to yield a Boolean from this Comparer, given a tuple of two Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the two Ts do not compare as the same.
    */
  def !=(tt: (T, T)): Boolean = ! ==(tt)

  /**
    * Method to apply a lens function U=>T to this Comparer[T], resulting in a Comparer[U].
    * The U values that are the inputs to the resulting Comparer, are passed to the lens function to
    * yield a suitable pair of T values which can then be passed into this Comparer.
    *
    * I originally considered this the infamous "unMap" method which is part of an un-monad.
    * Observe that the lens function is U=>T instead of T=>U which would be the parameter of a monadic map method.
    * In case you're wondering, an un-monad is the wrappee as opposed to the wrapper (which would be a monad).
    * However, I think it makes slightly more sense to call this snap because it creates something (kind of like a picture)
    * by using a lens.
    *
    * @param lens a function which takes a U and returns a T.
    * @tparam U the underlying type of the returned Comparer.
    * @return a Comparer[U].
    */
  def snap[U](lens: U => T): Comparer[U] = (uU: (U, U)) => self((lens(uU._1), lens(uU._2)))

  /**
    * A method to compose this Comparer with another Comparer of a different underlying type.
    *
    * @param uc a Comparer[U].
    * @tparam U the underlying type of uc.
    * @return a Comparer of tuples each comprising a T and a U.
    */
  def compose[U](uc: => Comparer[U]): Comparer[(T, U)] = (tut: ((T, U), (T, U))) => self(tut._1._1 -> tut._2._1) orElse uc(tut._1._2 -> tut._2._2)

  /**
    * Compose this Comparer with another Comparer of the same underlying type.
    *
    * @param tc the other Comparer (lazily evaluated).
    * @return the result of applying this Comparer unless it yields Same, in which case we invoke the other Comparer.
    */
  def orElse(tc: => Comparer[T]): Comparer[T] = (tt: (T, T)) => self(tt).orElse(tc(tt))

  /**
    * A non-monadic map method which maps this Comparer into a different Comparer,
    * but of the same underlying type.
    *
    * @param f the function which takes a Comparison and yields a different Comparison.
    * @return a new Comparer[T].
    */
  def map(f: Comparison => Comparison): Comparer[T] = (tt: (T, T)) => f(self(tt))

  /**
    * Method to invert the sense of a Comparer.
    *
    * @return a Compare[T] which, given the same tuple of Ts, yields the complementary Comparison to this Comparer.
    */
  def invert: Comparer[T] = map(_ flip)

  /**
    * Method to compose this Comparer[T] with a "lens" function that operates on a T.
    * See, for example, the definition of the Comparer in object DateF (in CompareSpec).
    *
    * The resulting Comparer[T] is formed by using orElse to compose this Comparer[T] with a Comparer[T] that:
    * is formed by using the "lens" function lens to snap (the implicit) comparer (which is a Comparer[U]) into a Comparer[T].
    *
    * This method is used primarily when chaining together several Comparers, each of which is derived from the given function
    * by invoking snap on an implicitly-defined Comparer, with the a specified function.
    * The initial value is typically provided by the "same" method of Comparer's companion object.
    *
    * @param lens a function which takes a T (the underlying type of this and the result) and returns a U.
    * @tparam U the underlying type of the implicit comparer.
    * @return a Comparer[T] which is composed from this and the unmapped form of comparer.
    */
  def :|[U: Comparer](lens: T => U): Comparer[T] = orElse(implicitly[Comparer[U]].snap(lens))
}

/**
  * Companion object for Comparer.
  * This is where you will find standard Comparer definitions.
  */
object Comparer {

  /**
    * A method to construct a Comparer which always evaluates to Same.
    * This is used, for example, in the apply method following.
    *
    * @tparam T the underlying type of the Comparer.
    * @return a Comparer[T] which always evaluates to Same.
    */
  def same[T]: Comparer[T] = new Comparer[T] { def apply(tt: (T, T)): Comparison = Same }

  /**
    * Method to construct a Comparer from a variable-length list of Comparers.
    * @param comparers the Comparers.
    * @tparam T the underlying type of all Comparers and the result.
    * @return a Comparer[T] which applies each Comparer in turn.
    */
  def create[T](comparers: Comparer[T]*): Comparer[T] = comparers.foldLeft[Comparer[T]](same)(_.orElse(_))

  /**
    * Method to construct a Comparer from a variable-length list of Lenses.
    * @param lenses the Lens functions, which must all be of the same type (T=>U).
    *               Note that if you want to have varying types, then use same and :| rather than apply.
    * @tparam T the underlying type of all Comparers and the result.
    * @return a Comparer[T] which applies each Comparer in turn.
    */
  def apply[T,U: Comparer](lenses: (T=>U)*): Comparer[T] = lenses.foldLeft[Comparer[T]](same)(_ :| _)

  /**
    * Following are the Comparer definitions for the common scalar types.
    */
  implicit val intComparer: Comparer[Int] = Ordering[Int]
  implicit val strComparer: Comparer[String] = Ordering[String]
  implicit val doubleComparer: Comparer[Double] = Ordering[Double]
  implicit val longComparer: Comparer[Long] = Ordering[Long]
  implicit val bigIntComparer: Comparer[BigInt] = Ordering[BigInt]

  /**
    * Implicit converter from Ordering[T] to Comparer[T].
    *
    * @param to the Ordering[T] to be converted.
    * @tparam T the underlying type of to and the result.
    * @return a Comparer[T] which has the same intrinsic behavior as "to".
    */
  implicit def convert[T](to: Ordering[T]): Comparer[T] = (tt: (T, T)) => Comparison(to.compare(tt._1, tt._2))
}

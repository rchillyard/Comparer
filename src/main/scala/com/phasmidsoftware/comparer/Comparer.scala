/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import scala.language.{implicitConversions, postfixOps}

/**
  * Type class trait Comparer[T].
  *
  * The behavior of this trait is that it is a lazy comparer of two T instances, presented as two curried parameters,
  * the inner (first) and the outer (second).
  * In all (curried) comparisons, the outer instance is compared with the inner instance.
  * The result of the comparison is a Comparison object.
  *
  * This class extends the function T => T => Comparison, i.e. a curried function.
  * This is more appropriate here because the two T values being compared are not related in any way--they
  * do not form part of something.
  * Furthermore, using a curried function allows us to yield a partially applied function which is a closure
  * on just one of the comparands.
  *
  * @tparam T the underlying type of the comparer. That's to say the type this Comparer knows about.
  */
trait Comparer[T] extends (T => T => Comparison) {
  self =>

  /**
    * Method to yield a Comparison from a tuple of Ts.
    * This, then, invokes the tupled version of apply.
    * This is essentially the same as the tupled method where the two input parameters are both Ts.
    *
    * @param tt a tuple (T,T).
    * @return a Comparison
    */
  def compare(tt: (T, T)): Comparison = self(tt._2)(tt._1)

  /**
    * Method to convert this Comparer[T] into an Ordering[T] which can then be used for more typical Java/Scala-style comparisons.
    *
    * @return a new Ordering[T].
    */
  def toOrdering: Ordering[T] = (t1: T, t2: T) => self(t2)(t1).toInt

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the left T is greater than the right T.
    */
  def >(t1: T)(t2: T): Boolean = self(t1)(t2).flip().getOrElse(false)

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the left T is less than the right T.
    */
  def <(t1: T)(t2: T): Boolean = self(t1)(t2)().getOrElse(false)

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the Ts compare as the same.
    */
  def ==(t1: T)(t2: T): Boolean = self(t1)(t2)() == Maybe

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the left T is greater than or equal to the right T.
    */
  def >=(t1: T)(t2: T): Boolean = ! <(t1)(t2)

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the left T is less than or equal to the right T.
    */
  def <=(t1: T)(t2: T): Boolean = ! >(t1)(t2)

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the two Ts do not compare as the same.
    */
  def !=(t1: T)(t2: T): Boolean = ! ==(t1)(t2)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
    * NOTE: When you use these tupled forms, the compiler doesn't need an extra set of parentheses.
    *
    * @param tt the tuple of Ts. Try saying that a few times!
    * @return a Boolean which is true if the left T is greater than the right T.
    */
  def >(tt: (T, T)): Boolean = >(tt._2)(tt._1)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the left T is less than the right T.
    */
  def <(tt: (T, T)): Boolean = <(tt._2)(tt._1)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the Ts compare as the same.
    */
  def ==(tt: (T, T)): Boolean = ==(tt._2)(tt._1)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the left T is greater than or equal to the right T.
    */
  def >=(tt: (T, T)): Boolean = ! <(tt)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the left T is less than or equal to the right T.
    */
  def <=(tt: (T, T)): Boolean = ! >(tt)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
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
  def snap[U](lens: U => T): Comparer[U] = u1 => u2 => self(lens(u1))(lens(u2))

  /**
    * A method to compose this Comparer with another Comparer of a different underlying type.
    *
    * @param uc a Comparer[U].
    * @tparam U the underlying type of uc.
    * @return a Comparer of tuples each comprising a T and a U.
    */
  def compose[U](uc: => Comparer[U]): Comparer[(T, U)] = tu1 => tu2 => self(tu1._1)(tu2._1) orElse uc(tu1._2)(tu2._2)

  /**
    * Compose this Comparer with another Comparer of the same underlying type.
    * If the result of applying this Comparer is Same, then Comparer tc is invoked.
    *
    * @param tc the other Comparer (lazily evaluated).
    * @return the result of applying this Comparer unless it yields Same, in which case we invoke the other Comparer.
    */
  def orElse(tc: => Comparer[T]): Comparer[T] = t1 => t2 => self(t1)(t2).orElse(tc(t1)(t2))

  /**
    * Compose this Comparer with another Comparer of the same underlying type.
    * If the result of applying this Comparer is Same, then Comparer tc is inverted and then invoked.
    *
    * @param tc the other Comparer (lazily evaluated) which is evaluated in the contrary sense.
    * @return the result of applying this Comparer unless it yields Same, in which case we invoke the inverse of the other Comparer.
    */
  def orElseNot(tc: => Comparer[T]): Comparer[T] = t1 => t2 => self(t1)(t2).orElse(tc.invert(t1)(t2))

  /**
    * A method which maps this Comparer into a different Comparer,
    * but of the same underlying type, according to the function f.
    *
    * NOTE: formerly called map.
    *
    * @param f the function which takes a Comparison and yields a different Comparison.
    * @return a new Comparer[T].
    */
  def transform(f: Comparison => Comparison): Comparer[T] = t1 => t2 => f(self(t1)(t2))

  /**
    * Method to invert the sense of a Comparer.
    *
    * @return a Compare[T] which, given the same tuple of Ts, yields the complementary Comparison to this Comparer.
    */
  def invert: Comparer[T] = transform(_ flip)

  /**
    * Method to compose this Comparer[T] with a Comparer[U] that is formed from the given "lens" function.
    * See, for example, the definition of the Comparer in object DateF (in CompareSpec).
    * Of all the methods which result in a new Comparer, this one is probably the most useful.
    *
    * The resulting Comparer[T] is formed by using orElse to compose this Comparer[T] with a Comparer[T] that:
    * is formed by using the "lens" function lens to snap (the implicit) comparer (which is a Comparer[U]) into a Comparer[T].
    *
    * This method is used primarily when chaining together several Comparers, each of which is derived from the given function
    * by invoking snap on an implicitly-defined Comparer, with the specified function.
    * The initial value is typically provided by the "same" method of Comparer's companion object.
    *
    * @param lens a function which takes a T (the underlying type of this and the result) and returns a U.
    * @tparam U the underlying type of the implicit comparer.
    * @return a Comparer[T] which is composed from this and the unmapped form of comparer.
    */
  def :|[U: Comparer](lens: T => U): Comparer[T] = orElse(implicitly[Comparer[U]].snap(lens))

  /**
    * Method to compose this Comparer[T] with a "lens" function that operates on a T.
    * See, for example, the definition of the Comparer in object DateF (in CompareSpec).
    *
    * The resulting Comparer[T] is formed by using orElseNot to compose this Comparer[T] with a Comparer[T] that:
    * is formed by using the "lens" function lens to snap (the implicit) comparer (which is a Comparer[U]) into a Comparer[T].
    *
    * This method is used primarily when chaining together several Comparers, each of which is derived from the given function
    * by invoking snap on an implicitly-defined Comparer, with the specified function.
    * The initial value is typically provided by the "same" method of Comparer's companion object.
    *
    * @param lens a function which takes a T (the underlying type of this and the result) and returns a U.
    * @tparam U the underlying type of the implicit comparer.
    * @return a Comparer[T] which is composed from this and the unmapped form of comparer.
    */
  def :|![U: Comparer](lens: T => U): Comparer[T] = orElseNot(implicitly[Comparer[U]].snap(lens))

  /**
    * Method to apply this Comparer to two (tupled) parameters rather than the curried form.
    * This is essentially the same as the compare method where the parameter is, explicitly, a (T,T).
    *
    * @param t1 the first T.
    * @param t2 the second T.
    * @return the resulting Comparison.
    */
  def tupled(t1: T, t2: T): Comparison = self(t2)(t1)
}

/**
  * Companion object for Comparer.
  * This is where you will find standard, implicit Comparer definitions.
  */
object Comparer {

  /**
    * A method to construct a Comparer which always evaluates to Same.
    * This is used, for example, in the apply method following.
    *
    * @tparam T the underlying type of the Comparer.
    * @return a Comparer[T] which always evaluates to Same.
    */
  def same[T]: Comparer[T] = _ => _ => Same

  /**
    * A method to construct a Comparer which always evaluates to Different(less).
    *
    * @tparam T the underlying type of the Comparer.
    * @return a Comparer[T] which always evaluates to Different(less).
    */
  def different[T](less: Boolean): Comparer[T] = _ => _ => Different(less)

  /**
    * Method to construct a Comparer from a variable-length list of Comparers.
    *
    * @param comparers the Comparers.
    * @tparam T the underlying type of all Comparers and the result.
    * @return a Comparer[T] which applies each Comparer in turn.
    */
  def create[T](comparers: Comparer[T]*): Comparer[T] = comparers.foldLeft[Comparer[T]](same)(_.orElse(_))

  /**
    * Method to construct a Comparer from a variable-length list of Lenses.
    *
    * @param lenses the Lens functions, which must all be of the same type (T=>U).
    *               Note that if you want to have varying types, then use same and :| rather than apply.
    * @tparam T the underlying type of all Comparers and the result.
    * @tparam U the type by which the actual comparisons will be made.
    * @return a Comparer[T] which applies each Comparer in turn.
    */
  def apply[T, U: Comparer](lenses: (T => U)*): Comparer[T] = lenses.foldLeft[Comparer[T]](same)(_ :| _)

  /**
    * Following are the Comparer definitions for the common scalar types.
    */
  implicit val unitComparer: Comparer[Unit] = Ordering[Unit]
  implicit val byteComparer: Comparer[Byte] = Ordering[Byte]
  implicit val charComparer: Comparer[Char] = Ordering[Char]
  implicit val shortComparer: Comparer[Short] = Ordering[Short]
  implicit val intComparer: Comparer[Int] = Ordering[Int]
  implicit val booleanComparer: Comparer[Boolean] = Ordering[Boolean]
  implicit val stringComparer: Comparer[String] = Ordering[String]
  implicit val doubleComparer: Comparer[Double] = Ordering.Double.TotalOrdering
  implicit val floatComparer: Comparer[Float] = Ordering.Float.TotalOrdering
  implicit val longComparer: Comparer[Long] = Ordering[Long]
  implicit val bigIntComparer: Comparer[BigInt] = Ordering[BigInt]
  implicit val bigDecimalComparer: Comparer[BigDecimal] = Ordering[BigDecimal]

  /**
    * Following are the Comparer definitions for the first eight tuple types (Tuple2 thru Tuple9).
    * If you are looking for comparisons of more complex tuples (with 10 or 11 parameters),
    * then define an explicit case class for your tuple (or you could just define a method which constructs your tuple and use that).
    * Then, you can use the mechanism of extending Comparers and invoking comparerN where N is the number of fields (up to N = 11)
    */
  implicit def tuple2[T1: Comparer, T2: Comparer]: Comparer[(T1, T2)] = Ordering.Tuple2[T1, T2](implicitly[Comparer[T1]].toOrdering, implicitly[Comparer[T2]].toOrdering)

  implicit def tuple3[T1: Comparer, T2: Comparer, T3: Comparer]: Comparer[(T1, T2, T3)] = Ordering.Tuple3[T1, T2, T3](implicitly[Comparer[T1]].toOrdering, implicitly[Comparer[T2]].toOrdering, implicitly[Comparer[T3]].toOrdering)

  implicit def tuple4[T1: Comparer, T2: Comparer, T3: Comparer, T4: Comparer]: Comparer[(T1, T2, T3, T4)] = Ordering.Tuple4[T1, T2, T3, T4](implicitly[Comparer[T1]].toOrdering, implicitly[Comparer[T2]].toOrdering, implicitly[Comparer[T3]].toOrdering, implicitly[Comparer[T4]].toOrdering)

  implicit def tuple5[T1: Comparer, T2: Comparer, T3: Comparer, T4: Comparer, T5: Comparer]: Comparer[(T1, T2, T3, T4, T5)] = Ordering.Tuple5[T1, T2, T3, T4, T5](implicitly[Comparer[T1]].toOrdering, implicitly[Comparer[T2]].toOrdering, implicitly[Comparer[T3]].toOrdering, implicitly[Comparer[T4]].toOrdering, implicitly[Comparer[T5]].toOrdering)

  implicit def tuple6[T1: Comparer, T2: Comparer, T3: Comparer, T4: Comparer, T5: Comparer, T6: Comparer]: Comparer[(T1, T2, T3, T4, T5, T6)] = Ordering.Tuple6[T1, T2, T3, T4, T5, T6](implicitly[Comparer[T1]].toOrdering, implicitly[Comparer[T2]].toOrdering, implicitly[Comparer[T3]].toOrdering, implicitly[Comparer[T4]].toOrdering, implicitly[Comparer[T5]].toOrdering, implicitly[Comparer[T6]].toOrdering)

  implicit def tuple7[T1: Comparer, T2: Comparer, T3: Comparer, T4: Comparer, T5: Comparer, T6: Comparer, T7: Comparer]: Comparer[(T1, T2, T3, T4, T5, T6, T7)] = Ordering.Tuple7[T1, T2, T3, T4, T5, T6, T7](implicitly[Comparer[T1]].toOrdering, implicitly[Comparer[T2]].toOrdering, implicitly[Comparer[T3]].toOrdering, implicitly[Comparer[T4]].toOrdering, implicitly[Comparer[T5]].toOrdering, implicitly[Comparer[T6]].toOrdering, implicitly[Comparer[T7]].toOrdering)

  implicit def tuple8[T1: Comparer, T2: Comparer, T3: Comparer, T4: Comparer, T5: Comparer, T6: Comparer, T7: Comparer, T8: Comparer]: Comparer[(T1, T2, T3, T4, T5, T6, T7, T8)] = Ordering.Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicitly[Comparer[T1]].toOrdering, implicitly[Comparer[T2]].toOrdering, implicitly[Comparer[T3]].toOrdering, implicitly[Comparer[T4]].toOrdering, implicitly[Comparer[T5]].toOrdering, implicitly[Comparer[T6]].toOrdering, implicitly[Comparer[T7]].toOrdering, implicitly[Comparer[T8]].toOrdering)

  implicit def tuple9[T1: Comparer, T2: Comparer, T3: Comparer, T4: Comparer, T5: Comparer, T6: Comparer, T7: Comparer, T8: Comparer, T9: Comparer]: Comparer[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = Ordering.Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicitly[Comparer[T1]].toOrdering, implicitly[Comparer[T2]].toOrdering, implicitly[Comparer[T3]].toOrdering, implicitly[Comparer[T4]].toOrdering, implicitly[Comparer[T5]].toOrdering, implicitly[Comparer[T6]].toOrdering, implicitly[Comparer[T7]].toOrdering, implicitly[Comparer[T8]].toOrdering, implicitly[Comparer[T9]].toOrdering)

  /**
    * Implicit converter from Ordering[T] to Comparer[T].
    *
    * @param to the Ordering[T] to be converted.
    * @tparam T the underlying type of to and the result.
    * @return a Comparer[T] which has the same intrinsic behavior as "to".
    */
  implicit def convert[T](to: Ordering[T]): Comparer[T] = t1 => t2 => Comparison.convert(to.compare(t2, t1))

  /**
    * This method creates a Comparer[T] based on an implicitly defined Comparer[P] and a lens function--
    * by invoking snap on the Comparer[P] and passing in lens.
    * It is used by the private comparer method in Comparers.
    *
    * @param lens a function which takes a T and returns a P.
    * @tparam T the underlying type of the returned Comparer.
    * @tparam P the result type of the lens function.
    * @return a Comparer[T].
    */
  def comparer[T, P: Comparer](lens: T => P): Comparer[T] = implicitly[Comparer[P]].snap(lens)
}

object Compare {
  /**
    * Method to construct a Comparison from two objects of type T (tupled form).
    * The sense of the result of this comparison is the same as,
    * for example, Double.compare(t1, t2).
    *
    * @param t1 the first T.
    * @param t2 the second T.
    * @tparam T the type of both t1 and t2, such type providing implicit evidence of a Comparer[T].
    * @return a Comparison, resulting from applying the comparer to the tuple of t1 and t2.
    */
  def apply[T: Comparer](t1: T, t2: T): Comparison = Comparison(t2)(t1)
}
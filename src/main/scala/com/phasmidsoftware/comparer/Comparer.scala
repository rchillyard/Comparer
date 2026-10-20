/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import scala.collection.immutable.ArraySeq
import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror
import scala.util.Try

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
  * A Comparer for a case class can be derived, comparing the fields in order from most to least significant:
  * {{{
  *   case class Date(year: Int, month: Int, day: Int) derives Comparer
  * }}}
  *
  * @tparam T the underlying type of the comparer. That's to say the type this Comparer knows about.
  */
trait Comparer[T] extends (T => T => Comparison):

  /**
    * Method to yield a Comparison from a tuple of Ts.
    * This, then, invokes the tupled version of apply.
    * This is essentially the same as the tupled method where the two input parameters are both Ts.
    *
    * @param tt a tuple (T,T).
    * @return a Comparison
    */
  def compare(tt: (T, T)): Comparison =
    this(tt._2)(tt._1)

  /**
    * Method to convert this Comparer[T] into an Ordering[T] which can then be used for more typical Java/Scala-style comparisons.
    *
    * @return a new Ordering[T].
    */
  def toOrdering: Ordering[T] =
    (t1: T, t2: T) => this(t2)(t1).toInt

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the left T is greater than the right T.
    */
  def >(t1: T)(t2: T): Boolean =
    this(t1)(t2) == Comparison.More

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the left T is less than the right T.
    */
  def <(t1: T)(t2: T): Boolean =
    this(t1)(t2) == Comparison.Less

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the Ts compare as the same.
    */
  def ==(t1: T)(t2: T): Boolean =
    this(t1)(t2) == Same

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the left T is greater than or equal to the right T.
    */
  def >=(t1: T)(t2: T): Boolean =
    ! <(t1)(t2)

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the left T is less than or equal to the right T.
    */
  def <=(t1: T)(t2: T): Boolean =
    ! >(t1)(t2)

  /**
    * Method to yield a Boolean from this Comparer, given two curried Ts.
    *
    * @param t1 the first value of T.
    * @param t2 the second value of T.
    * @return a Boolean which is true if the two Ts do not compare as the same.
    */
  def !=(t1: T)(t2: T): Boolean =
    ! ==(t1)(t2)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
    * NOTE: When you use these tupled forms, the compiler doesn't need an extra set of parentheses.
    *
    * @param tt the tuple of Ts. Try saying that a few times!
    * @return a Boolean which is true if the left T is greater than the right T.
    */
  def >(tt: (T, T)): Boolean =
    >(tt._2)(tt._1)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the left T is less than the right T.
    */
  def <(tt: (T, T)): Boolean =
    <(tt._2)(tt._1)

  /**
    * Method to yield a Boolean from this Comparer, given two tupled Ts.
    *
    * @param tt the tuple of Ts.
    * @return a Boolean which is true if the Ts compare as the same.
    */
  def ==(tt: (T, T)): Boolean =
    ==(tt._2)(tt._1)

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
  infix def snap[U](lens: U => T): Comparer[U] =
    u1 => u2 => this(lens(u1))(lens(u2))

  /**
    * A method to compose this Comparer with another Comparer of a different underlying type.
    *
    * @param uc a Comparer[U].
    * @tparam U the underlying type of uc.
    * @return a Comparer of tuples each comprising a T and a U.
    */
  infix def compose[U](uc: => Comparer[U]): Comparer[(T, U)] =
    tu1 => tu2 => this(tu1._1)(tu2._1) orElse uc(tu1._2)(tu2._2)

  /**
    * Compose this Comparer with another Comparer of the same underlying type.
    * If the result of applying this Comparer is Same, then Comparer tc is invoked.
    *
    * @param tc the other Comparer (lazily evaluated).
    * @return the result of applying this Comparer unless it yields Same, in which case we invoke the other Comparer.
    */
  infix def orElse(tc: => Comparer[T]): Comparer[T] =
    t1 => t2 => this(t1)(t2) orElse tc(t1)(t2)

  /**
    * Compose this Comparer with another Comparer of the same underlying type.
    * If the result of applying this Comparer is Same, then Comparer tc is inverted and then invoked.
    *
    * @param tc the other Comparer (lazily evaluated) which is evaluated in the contrary sense.
    * @return the result of applying this Comparer unless it yields Same, in which case we invoke the inverse of the other Comparer.
    */
  infix def orElseNot(tc: => Comparer[T]): Comparer[T] =
    t1 => t2 => this(t1)(t2) orElse tc.invert(t1)(t2)

  /**
    * A method which maps this Comparer into a different Comparer,
    * but of the same underlying type, according to the function f.
    *
    * NOTE: formerly called map.
    *
    * @param f the function that takes a Comparison and yields a different Comparison.
    * @return a new Comparer[T].
    */
  def transform(f: Comparison => Comparison): Comparer[T] =
    t1 => t2 => f(this(t1)(t2))

  /**
    * Method to invert the sense of a Comparer.
    *
    * @return a Comparer[T] which, given the same tuple of Ts, yields the complementary Comparison to this Comparer.
    */
  def invert: Comparer[T] = transform(_.flip)

  /**
    * Method to compose this Comparer[T] with a Comparer[U] that is formed from the given "lens" function.
    * See, for example, the definition of the Comparer in object DateF (in ComparerSpec).
    * Of all the methods which result in a new Comparer, this one is probably the most useful.
    *
    * The resulting Comparer[T] is formed by using orElse to compose this Comparer[T] with a Comparer[T] that:
    * is formed by using the "lens" function lens to snap the given Comparer[U] into a Comparer[T].
    *
    * This method is used primarily when chaining together several Comparers, each of which is derived from the given function
    * by invoking snap on a given Comparer, with the specified function.
    * The initial value is typically provided by the "same" method of Comparer's companion object.
    *
    * @param lens a function that takes a T (the underlying type of this and the result) and returns a U.
    * @tparam U the underlying type of the given comparer.
    * @return a Comparer[T] which is composed of this and the unmapped form of comparer.
    */
  def :|[U: Comparer](lens: T => U): Comparer[T] =
    orElse(summon[Comparer[U]].snap(lens))

  /**
    * Method to compose this Comparer[T] with a "lens" function that operates on a T.
    * See, for example, the definition of the Comparer in object DateF (in ComparerSpec).
    *
    * The resulting Comparer[T] is formed by using orElseNot to compose this Comparer[T] with a Comparer[T] that:
    * is formed by using the "lens" function lens to snap the given Comparer[U] into a Comparer[T].
    *
    * This method is used primarily when chaining together several Comparers, each of which is derived from the given function
    * by invoking snap on a given Comparer, with the specified function.
    * The initial value is typically provided by the "same" method of Comparer's companion object.
    *
    * @param lens a function that takes a T (the underlying type of this and the result) and returns a U.
    * @tparam U the underlying type of the given comparer.
    * @return a Comparer[T] which is composed of this and the unmapped form of comparer.
    */
  def :|![U: Comparer](lens: T => U): Comparer[T] =
    orElseNot(summon[Comparer[U]].snap(lens))

  /**
    * Method to apply this Comparer to two (tupled) parameters rather than the curried form.
    * This is essentially the same as the compare method where the parameter is, explicitly, a (T,T).
    *
    * @param t1 the first T.
    * @param t2 the second T.
    * @return the resulting Comparison.
    */
  def tupled(t1: T, t2: T): Comparison = this(t2)(t1)

/**
  * Companion object for Comparer.
  * This is where you will find the standard given Comparer instances,
  * which are therefore available without any import.
  */
object Comparer:

  /**
    * A method to construct a Comparer which always evaluates to Same.
    * This is used, for example, in the apply method following.
    *
    * @tparam T the underlying type of the Comparer.
    * @return a Comparer[T] which always evaluates to Same.
    */
  def same[T]: Comparer[T] =
    _ => _ => Same

  /**
    * A method to construct a Comparer which always evaluates to Different(less).
    *
    * @tparam T the underlying type of the Comparer.
    * @return a Comparer[T] which always evaluates to Different(less).
    */
  def different[T](less: Boolean): Comparer[T] =
    _ => _ => Different(less)

  /**
    * Method to construct a Comparer from a variable-length list of Comparers.
    *
    * @param comparers the Comparers.
    * @tparam T the underlying type of all Comparers and the result.
    * @return a Comparer[T] which applies each Comparer in turn.
    */
  def create[T](comparers: Comparer[T]*): Comparer[T] =
    comparers.foldLeft(same[T])(_ orElse _)

  /**
    * Method to construct a Comparer from a variable-length list of Lenses.
    *
    * @param lenses the Lens functions, which must all be of the same type (T=>U).
    *               Note that if you want to have varying types, then use same and :| rather than apply.
    * @tparam T the underlying type of all Comparers and the result.
    * @tparam U the type by which the actual comparisons will be made.
    * @return a Comparer[T] which applies each Comparer in turn.
    */
  def apply[T, U: Comparer](lenses: (T => U)*): Comparer[T] =
    lenses.foldLeft(same[T])(_ :| _)

  /**
    * Method to create a Comparer[T] from an Ordering[T].
    *
    * @param to the Ordering[T] to be converted.
    * @tparam T the underlying type of to and the result.
    * @return a Comparer[T] which has the same intrinsic behavior as "to".
    */
  def fromOrdering[T](to: Ordering[T]): Comparer[T] =
    t1 => t2 => Comparison.convert(to.compare(t2, t1))

  /**
    * This method creates a Comparer[T] based on a given Comparer[P] and a lens function--
    * by invoking snap on the Comparer[P] and passing in lens.
    *
    * @param lens a function which takes a T and returns a P.
    * @tparam T the underlying type of the returned Comparer.
    * @tparam P the result type of the lens function.
    * @return a Comparer[T].
    */
  def comparer[T, P: Comparer](lens: T => P): Comparer[T] = summon[Comparer[P]].snap(lens)

  /**
    * Method to derive a Comparer, so that you can write, for a case class:
    * {{{
    *   case class Date(year: Int, month: Int, day: Int) derives Comparer
    * }}}
    * or, for an enum (or a sealed trait):
    * {{{
    *   enum Suit derives Comparer:
    *     case Clubs, Diamonds, Hearts, Spades
    * }}}
    *
    * For a case class (or any other product type), the fields are compared in order,
    * from the first (most significant) to the last (least significant),
    * and each field type must itself have a given Comparer.
    * If you need a different order, then build the Comparer explicitly, using same and :|.
    *
    * For an enum or a sealed trait (a sum type), values of different cases are compared by the order in which the cases are declared.
    * Values of the same case are compared using that case's given Comparer if it has one,
    * otherwise using a Comparer derived for that case (i.e. by its fields).
    *
    * Recursive types (such as a tree whose nodes contain trees) are supported.
    *
    * @param m the Mirror of T, supplied by the compiler.
    * @tparam T the underlying type of the result.
    * @return a Comparer[T].
    */
  inline def derived[T](using m: Mirror.Of[T]): Comparer[T] = inline m match
    case s: Mirror.SumOf[T] => sumComparer(s, caseComparers[s.MirroredElemTypes])
    case p: Mirror.ProductOf[T] => productComparer(p, summonInline[Comparer[p.MirroredElemTypes]])

  /**
    * Method to create a Comparer for a product type from the Comparer for the tuple of its field types.
    *
    * NOTE: a type with a Mirror.ProductOf (a case class, case object or enum case) is always a Product.
    *
    * @param p  the Mirror of T.
    * @param ec the Comparer of the tuple of T's field types (by-name, in case T is recursive).
    * @tparam T the product type.
    * @return a Comparer[T].
    */
  private def productComparer[T](p: Mirror.ProductOf[T], ec: => Comparer[p.MirroredElemTypes]): Comparer[T] =
    lazy val fieldsComparer = ec
    def fields(t: T): p.MirroredElemTypes = Tuple.fromProduct(t.asInstanceOf[Product]).asInstanceOf[p.MirroredElemTypes]
    t1 => t2 => fieldsComparer(fields(t1))(fields(t2))

  /**
    * Method to create a Comparer for a sum type (an enum or a sealed trait).
    *
    * NOTE: the cast is safe because the case Comparer is only used when both values have the same ordinal, i.e. are of that case.
    *
    * @param s     the Mirror of T.
    * @param cases the Comparers of the cases of T, in ordinal order (by-name, in case T is recursive).
    * @tparam T the sum type.
    * @return a Comparer[T].
    */
  private def sumComparer[T](s: Mirror.SumOf[T], cases: => List[Comparer[?]]): Comparer[T] =
    lazy val caseComparers = cases.toVector
    t1 => t2 =>
      val (o1, o2) = (s.ordinal(t1), s.ordinal(t2))
      intComparer(o1)(o2) orElse caseComparers(o1).asInstanceOf[Comparer[T]](t1)(t2)

  /**
    * Method to get a Comparer for each of the types in Ts (the cases of a sum type).
    *
    * @tparam Ts a tuple of types.
    * @return a list of Comparers, in the same order as Ts.
    */
  private inline def caseComparers[Ts <: Tuple]: List[Comparer[?]] = inline erasedValue[Ts] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => caseComparer[t] :: caseComparers[ts]

  /**
    * Method to get a Comparer for A: the given one if there is one; otherwise one derived for A.
    *
    * @tparam A the type of one case of a sum type.
    * @return a Comparer[A].
    */
  private inline def caseComparer[A]: Comparer[A] = summonFrom:
    case c: Comparer[A] => c
    case _ => derived[A](using summonInline[Mirror.Of[A]])

  /**
    * Following are the Comparer definitions for the common scalar types.
    */
  given unitComparer: Comparer[Unit] = fromOrdering(Ordering[Unit])
  given byteComparer: Comparer[Byte] = fromOrdering(Ordering[Byte])
  given charComparer: Comparer[Char] = fromOrdering(Ordering[Char])
  given shortComparer: Comparer[Short] = fromOrdering(Ordering[Short])
  given intComparer: Comparer[Int] = fromOrdering(Ordering[Int])
  given booleanComparer: Comparer[Boolean] = fromOrdering(Ordering[Boolean])
  given stringComparer: Comparer[String] = fromOrdering(Ordering[String])
  given doubleComparer: Comparer[Double] = fromOrdering(Ordering.Double.TotalOrdering)
  given floatComparer: Comparer[Float] = fromOrdering(Ordering.Float.TotalOrdering)
  given longComparer: Comparer[Long] = fromOrdering(Ordering[Long])
  given bigIntComparer: Comparer[BigInt] = fromOrdering(Ordering[BigInt])
  given bigDecimalComparer: Comparer[BigDecimal] = fromOrdering(Ordering[BigDecimal])

  /**
    * The Comparer for the empty tuple, which ends the recursion of tupleComparer.
    */
  given emptyTupleComparer: Comparer[EmptyTuple] = same

  /**
    * The Comparer for a tuple of any size whose elements all have given Comparers.
    * The elements are compared in order, from the first (most significant) to the last (least significant).
    * This also provides the comparison of the fields for derived Comparers.
    *
    * NOTE: hc and rc are by-name so that a recursive type can refer to its own Comparer (which is still being defined).
    *
    * @tparam H the type of the first element.
    * @tparam R the type of the tuple of the remaining elements.
    * @return a Comparer[H *: R].
    */
  given tupleComparer[H, R <: Tuple](using hc: => Comparer[H], rc: => Comparer[R]): Comparer[H *: R] =
    lazy val headComparer = hc
    lazy val restComparer = rc
    t1 => t2 => headComparer(t1.head)(t2.head) orElse restComparer(t1.tail)(t2.tail)

  /**
    * The Comparer of Iterable[T] where T is any type that has a given Comparer.
    * Elements at the head of an iterable are considered more significant than those appearing later;
    * Comparisons are made only through the shortest iterable.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Iterable[T] which can compare two instances of Iterable[T] and return a Comparison.
    */
  given comparerIterable[T: Comparer]: Comparer[Iterable[T]] = ts2 => ts1 => comparePrefixes(ts1, ts2)

  /**
    * The Comparer of Seq[T] where T is any type that has a given Comparer (see comparerIterable).
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Seq[T].
    */
  given comparerSeq[T: Comparer]: Comparer[Seq[T]] = comparerIterable[T].snap(identity)

  /**
    * The Comparer of List[T] where T is any type that has a given Comparer (see comparerIterable).
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of List[T].
    */
  given comparerList[T: Comparer]: Comparer[List[T]] = comparerIterable[T].snap(identity)

  /**
    * The Comparer of Array[T] where T is any type that has a given Comparer (see comparerIterable).
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Array[T].
    */
  given comparerArray[T: Comparer]: Comparer[Array[T]] = comparerIterable[T].snap(ArraySeq.unsafeWrapArray)

  /**
    * The Comparer of Option[T] where T is any type that has a given Comparer.
    * NOTE that we arbitrarily evaluate all comparisons involving None to be Same.
    * NOTE this behavior is NOT the same as when using OptionOrdering[T] defined in Ordering.scala.
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Option[T] which can compare two instances of Option[T] and return a Comparison.
    */
  given comparerOpt[T: Comparer]: Comparer[Option[T]] = to1 => to2 =>
    (for t1 <- to1; t2 <- to2 yield summon[Comparer[T]](t1)(t2)).getOrElse(Same)

  /**
    * The Comparer of Try[T] where T is any type that has a given Comparer.
    * NOTE that we arbitrarily evaluate all comparisons involving Failure to be Same (see comparerOpt).
    *
    * @tparam T the underlying type of the inputs.
    * @return a Comparer of Try[T] which can compare two instances of Try[T] and return a Comparison.
    */
  given comparerTry[T: Comparer]: Comparer[Try[T]] = comparerOpt[T].snap(_.toOption)

  /**
    * The Comparer of Either[L,R] where R is any type that has a given Comparer.
    * Note that we arbitrarily evaluate all comparisons involving L to be Same (see comparerOpt).
    *
    * @tparam L the underlying left-hand type of the inputs.
    * @tparam R the underlying right-hand type of the inputs (must have a given Comparer).
    * @return a Comparer of Either[L,R] which can compare two instances of Either[L,R] and return a Comparison.
    */
  given comparerEither[L, R: Comparer]: Comparer[Either[L, R]] = comparerOpt[R].snap(_.toOption)

  /**
    * Method to compare two Iterables such that only the common prefixes are compared.
    *
    * @param ts1 an Iterable[T].
    * @param ts2 an Iterable[T].
    * @tparam T the underlying type.
    * @return a Comparison.
    */
  def comparePrefixes[T: Comparer](ts1: Iterable[T], ts2: Iterable[T]): Comparison =
    (ts1 zip ts2).foldLeft(Same: Comparison):
      case (c, (t1, t2)) => c orElse Compare(t1, t2)

  /**
    * Method to compare two Iterables such that, if the common prefixes are the same,
    * the "larger" Iterable is considered to be the longer one.
    *
    * @param ts1 an Iterable[T].
    * @param ts2 an Iterable[T].
    * @tparam T the underlying type.
    * @return a Comparison.
    */
  def compareAll[T: Comparer](ts1: Iterable[T], ts2: Iterable[T]): Comparison =
    comparePrefixes(ts1, ts2) orElse intComparer.compare((ts1.size, ts2.size))

object Compare:
  /**
    * Method to construct a Comparison from two objects of type T (tupled form).
    * The sense of the result of this comparison is the same as,
    * for example, Double.compare(t1, t2).
    *
    * @param t1 the first T.
    * @param t2 the second T.
    * @tparam T the type of both t1 and t2, such type providing a given Comparer[T].
    * @return a Comparison, resulting from applying the comparer to the tuple of t1 and t2.
    */
  def apply[T: Comparer](t1: T, t2: T): Comparison = Comparison(t2)(t1)

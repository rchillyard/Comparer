/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

/**
  * Enumeration which embodies the behavior of a lazy, three-way comparison.
  *
  * The problem with the Java style of comparing, which Scala also uses, is that you cannot easily compose
  * comparisons. A typical comparison of two quantities compares the two most significant quantities, then,
  * if the objects appear different, the result is returned; otherwise the result of the comparison of the
  * less significant quantities is returned. This requires saving the value of the first comparison in a (final) variable.
  * But yuck!
  *
  * Although it would be possible to write this eagerly, or simply replace it with an Option[Boolean],
  * there is some slight advantage in defining it lazily.
  * In many situations we use pattern-matching rather than evaluation of the Comparison value.
  *
  * NOTE: also that Comparison is a form of three-valued logic.
  * which does not correspond exactly to the the indeterminate state.
  * We can generate a Kleenean object by invoking the Comparison's apply method.
  *
  * NOTE: the cases are exported at the package level, so that they can be referred to simply as Same and Different.
  */
enum Comparison extends Thunk[Kleenean] {

  /**
    * Sameness (two objects compare as equal).
    */
  case Same

  /**
    * A Comparison which is different.
    *
    * @param less true or false. By conventions, we yield a true value when we compare a lesser object with a greater object.
    */
  case Different(less: Boolean)

  /**
    * Method to eagerly evaluate this Comparison.
    *
    * @return Maybe if this is Same, otherwise Truth(less).
    */
  def apply(): Kleenean = this match {
    case Same => Maybe
    case Different(less) => Truth(less)
  }

  /**
    * Method to yield logical AND.
    *
    * @param c the other Comparison (eagerly evaluated).
    * @return a Comparison according to Kleenean logic.
    */
  def &(c: Comparison): Comparison = Comparison.convert(math.min(toInt, c.toInt))

  /**
    * Method to yield logical OR.
    *
    * @param c the other Comparison (eagerly evaluated).
    * @return a Comparison according to Kleenean logic.
    */
  def |(c: Comparison): Comparison = Comparison.convert(math.max(toInt, c.toInt))

  /**
    * Method to yield logical AND with short-circuit logic.
    *
    * @param c the other Comparison (lazily evaluated: not evaluated if this is Different(true)).
    * @return a Comparison according to Kleenean logic.
    */
  def &&(c: => Comparison): Comparison = this match {
    case Different(true) => this
    case Different(false) => c
    case Same => c match {
      case less@Different(true) => less
      case _ => Same
    }
  }

  /**
    * Method to yield logical OR with short-circuit logic.
    *
    * @param c the other Comparison (lazily evaluated: not evaluated if this is Different(false)).
    * @return a Comparison according to Kleenean logic.
    */
  def ||(c: => Comparison): Comparison = this match {
    case Different(false) => this
    case Different(true) => c
    case Same => c match {
      case more@Different(false) => more
      case _ => Same
    }
  }

  /**
    * Method to return the Java-style value of this Comparison.
    *
    * @return if Same then 0 else if Different(true) then -1 else 1
    */
  def toInt: Int = this match {
    case Same => 0
    case Different(less) => if (less) -1 else 1
  }

  /**
    * Method to compose this with another Comparison.
    * That is to say we yield either this or, in the case that this is Same, a default value of Comparison.
    *
    * @param c the other Comparison (lazily evaluated: only evaluated if this is Same).
    * @return the composition of this and c.
    */
  infix def orElse(c: => Comparison): Comparison = this match {
    case Same => c
    case _ => this
  }

  /**
    * Method to yield the complementary Comparison to this Comparison, that's to say the result is flipped (i.e. negated).
    *
    * @return Same if this Comparison is Same else the complementary (flipped) value of Different.
    */
  def flip: Comparison = this match {
    case Same => Same
    case Different(less) => Different(!less)
  }

  /**
    * Method to yield a String representing this Comparison.
    *
    * @return "Same", "Less" or "More".
    */
  override def toString(): String = this match {
    case Same => "Same"
    case Different(less) => if (less) "Less" else "More"
  }
}

/**
  * Companion object for Comparison.
  */
object Comparison {
  /**
    * Different(false).
    */
  val More: Comparison = Different(false)
  /**
    * Different(true).
    */
  val Less: Comparison = Different(true)

  /**
    * Method to construct a Comparison from a Boolean.
    *
    * @param b true or false.
    * @return Different(b)
    */
  def apply(b: Boolean): Comparison = Different(b)

  /**
    * Method to construct a Comparison from a Kleenean.
    *
    * @param k a Kleenean.
    * @return the homologous Comparison for the input.
    */
  def apply(k: Kleenean): Comparison = k match {
    case Truth(b) => Different(b)
    case Maybe => Same
  }

  /**
    * Method to construct a Comparison from an Option[Boolean].
    *
    * @param bo an optional Boolean.
    * @return the homologous Comparison for the input.
    */
  def apply(bo: Option[Boolean]): Comparison = bo.fold(Same)(Different(_))

  /**
    * Method to construct a Comparison from a Java-style comparison result.
    *
    * @param x an integer which is either less than 0, equal to 0, or greater than 0.
    *          Typically, this is the result of a Java-style comparison.
    * @return the homologous Comparison for x, either Same or Different(b) where b is true if x is negative.
    */
  def convert(x: Int): Comparison = if (x == 0) Same else Different(x < 0)

  /**
    * Method to construct a Comparison from two objects of type T (curried form).
    *
    * NOTE: the sense of the result will be similar to invoking, for example, Double.compare(t1, t2).
    *
    * @param t2 the inner T.
    * @param t1 the outer T.
    * @tparam T the type of both t1 and t2, which type must also provide a given Comparer[T].
    * @return a Comparison, resulting from applying the comparer to t1 and its result to t2.
    */
  def apply[T: Comparer](t2: T)(t1: T): Comparison = summon[Comparer[T]](t2)(t1)
}

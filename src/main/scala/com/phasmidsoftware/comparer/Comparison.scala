/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import scala.language.{implicitConversions, postfixOps}

/**
  * Function trait which embodies the behavior of a lazy (three-way) comparison.
  *
  * The problem with the Java style of comparing, which Scala also uses, is that you cannot easily compose
  * comparisons. A typical comparison of two quantities compares the two most significant quantities, then,
  * if the objects appear different, the result is returned; otherwise the result of the comparison of the
  * less significant quantities is returned. This requires saving the value of the first comparison in a (final) variable.
  * But yuck!
  *
  * Although it would be possible to write this eagerly, or simply replace it with an Option[Boolean],
  * there is some slight advantage in defining it lazily.
  *
  * This trait is sealed because there should be only two concrete sub-types.
  */
sealed trait Comparison extends (() => Option[Boolean]) {

  /**
    * Method to eagerly evaluate this Comparison.
    *
    * @return an Option[Boolean].
    */
  def apply(): Option[Boolean]

  /**
    * Method to yield logical AND.
    *
    * @param c the other Comparison (eagerly evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def &(c: Comparison): Comparison = Comparison(math.min(toInt, c.toInt))

  /**
    * Method to yield logical OR.
    *
    * @param c the other Comparison (eagerly evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def |(c: Comparison): Comparison = Comparison(math.max(toInt, c.toInt))

  /**
    * Method to yield logical AND with short-circuit logic.
    *
    * @param c the other Comparison (lazily evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def &&(c: => Comparison): Comparison

  /**
    * Method to yield logical OR with short-circuit logic.
    *
    * @param c the other Comparison (lazily evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def ||(c: => Comparison): Comparison

  /**
    * Method to yield a String representing this Comparison.
    *
    * @return a String formed from the evaluated Comparison.
    */
  override def toString(): String = ().toString

  /**
    * Method to return the Java-style value of this Comparison
    *
    * @return if Same then 0 else if Different(true) then -1 else 1
    */
  def toInt: Int

  /**
    * Method to compose this with another Comparison.
    * That is to say we yield either this or, in the case that this is Same, a default value of Comparison.
    *
    * @param c the other Comparison (lazily evaluated).
    * @return the composition of this and c.
    */
  def orElse(c: => Comparison): Comparison = Comparison(apply.orElse(c()))

  /**
    * Method to yield the complementary Comparison to this Comparison, that's to say the result is flipped (i.e. negated).
    *
    * @return Same if this Comparison is Same else the complementary (flipped) value of Different.
    */
  def flip: Comparison
}

/**
  * Case class which represents a Comparison which is different.
  *
  * @param less true or false. By conventions, we yield a true value when we compare a lesser object with a greater object.
  */
case class Different(less: Boolean) extends Comparison {
  /**
    * Eagerly evaluate this Different.
    *
    * @return Some(less).
    */
  def apply(): Option[Boolean] = Some(less)

  /**
    *
    * @param c the other Comparison (lazily evaluated)
    * @return a Comparison according to Kleenean logic
    */
  def &&(c: => Comparison): Comparison = if (less) this else c

  def ||(c: => Comparison): Comparison = if (less) c else this

  /**
    * @return Different(!less).
    */
  def flip: Comparison = Different(!less)

  override def toInt: Int = if (less) -1 else 1
}

/**
  * Case class which represents sameness (two objects compare as equal).
  */
case object Same extends Comparison {
  /**
    * Eagerly evaluate this Same.
    *
    * @return an Option[Boolean].
    */
  def apply(): Option[Boolean] = None

  def &&(c: => Comparison): Comparison = c & this

  def ||(c: => Comparison): Comparison = c | this

  def flip: Comparison = this

  override def toInt: Int = 0
}

/**
  * Companion object for Comparison.
  */
object Comparison {
  val more: Comparison = Different(false)
  val less: Comparison = Different(true)

  def apply(b: Boolean): Comparison = Different(b)

  def apply(x: Option[Boolean]): Comparison = x match {
    case Some(b) => apply(b);
    case _ => Same
  }

  def apply(x: Int): Comparison = x match {
    case 0 => Same;
    case _ => Comparison(Some(x < 0))
  }
}

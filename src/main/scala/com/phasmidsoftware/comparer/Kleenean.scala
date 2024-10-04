/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

/**
  * Trait which models a three-valued logic based on the algebra of Stephen C. Kleene.
  * See https://en.wikipedia.org/wiki/Three-valued_logic
  *
  * A Kleenean can be lazily converted to an Option[Boolean] when required.
  */
sealed trait Kleenean extends (() => Option[Boolean]) {

  /**
    * Yield an Int corresponding to a Java-style comparison for this Kleenean.
    *
    * @return an Int which is either -1, 0 or 1.
    */
  def toInt: Int

  /**
    * Return the logical OR of this and k.
    *
    * @param k the other Kleenean (call-by-name).
    * @return the logical OR.
    */
  def |(k: => Kleenean): Kleenean

  /**
    * Return the logical AND of this and k.
    *
    * @param k the other Kleenean (call-by-name).
    * @return the logical AND.
    */
  def &(k: => Kleenean): Kleenean

  /**
    * Negate this Kleenean.
    *
    * @return the complement of this Kleenean.
    */
  def ! : Kleenean

  /**
    * Convert this Kleenean into a Boolean by providing a default value for the Maybe case.
    *
    * @param x the result if this is Maybe.
    * @return a Boolean corresponding to this Kleenean.
    */
  def getOrElse(x: => Boolean): Boolean
}

/**
  * The indeterminate state of a Kleenean, neither true nor false.
  */
case object Maybe extends Kleenean {

  /**
    * Apply method to convert this Kleenean to an Option[Boolean].
    * NOTE do not define as a lazy val.
    *
    * @return None
    */
  def apply(): Option[Boolean] = None

  /**
    * @return 0.
    */
  lazy val toInt: Int = 0

  /**
    * The logical OR of this and k.
    *
    * Equivalent to
    * <code>
    * val int = k.toInt
    * if (int == 0) this else Kleenean(math.max(toInt, int))
    * </code>
    *
    * @param k the other Kleenean (always evaluated).
    * @return the logical OR.
    */
  def |(k: => Kleenean): Kleenean =
    k match {
      case Truth(true) => k
      case _ => this
    }

  /**
    * The logical AND of this and k.
    *
    * Equivalent to
    * <code>
    * val int = k.toInt
    * if (int == 0) this else Kleenean(math.min(toInt, int))
    * </code>
    *
    * @param k the other Kleenean (always evaluated).
    * @return the logical AND.
    */
  def &(k: => Kleenean): Kleenean = k match {
    case Truth(false) => k
    case _ => this
  }

  override def toString(): String = "?"

  /**
    * Convert this Kleenean into a Boolean by providing a default value for the Maybe case.
    *
    * @param x the result.
    * @return x.
    */
  def getOrElse(x: => Boolean): Boolean = x

  /**
    * Negate this Maybe: return Maybe.
    *
    * @return Maybe.
    */
  lazy val ! : Kleenean = Maybe
}

/**
  * The Boolean equivalent of a Kleenean.
  *
  * @param b either true or false.
  */
case class Truth(b: Boolean) extends Kleenean {

  /**
    * Apply method to convert this Truth value to an Option[Boolean].
    * NOTE do not define as a lazy val.
    *
    * @return Some(b)
    */
  def apply(): Option[Boolean] = Some(b)

  /**
    * @return an Int which is -1 if b is false, otherwise 1.
    */
  lazy val toInt: Int = if (b) 1 else -1

  /**
    * @param k the other Kleenean (call-by-name, only evaluated if b is false).
    * @return the logical OR.
    */
  def |(k: => Kleenean): Kleenean = if (b) this else k

  /**
    * @param k the other Kleenean (call-by-name, only evaluated if b is true).
    * @return the logical AND.
    */
  def &(k: => Kleenean): Kleenean = if (b) k else this

  /**
    * Convert this Kleenean into a Boolean by providing a default value for the Maybe case.
    *
    * @param x ignored.
    * @return a Boolean corresponding to this Truth value.
    */
  def getOrElse(x: => Boolean): Boolean = b

  /**
    * Negate this Truth value.
    *
    * @return the complement of this Kleenean.
    */
  lazy val ! : Kleenean = Truth(!b)

  override def toString(): String = if (b) "T" else "F"
}

object Kleenean {
  /**
    * Construct a Truth value given the input b.
    *
    * @param b a Boolean.
    * @return a Kleenean.
    */
  def apply(b: Boolean): Kleenean = Truth(b)

  /**
    * Construct a Kleenean according to the given value x.
    *
    * @param x an Int.
    * @return if x==0 then Maybe else apply(x>0)
    */
  def apply(x: Int): Kleenean = if (x == 0) Maybe else apply(x > 0)
}
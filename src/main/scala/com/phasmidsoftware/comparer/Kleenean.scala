/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

/**
  * Enumeration which models a three-valued logic based on the algebra of Stephen C. Kleene.
  * See https://en.wikipedia.org/wiki/Three-valued_logic
  *
  * A Kleenean can be lazily converted to an Option[Boolean] when required.
  *
  * NOTE: the cases are exported at the package level, so that they can be referred to simply as Maybe and Truth.
  */
enum Kleenean extends Thunk[Option[Boolean]]:

  /**
    * The indeterminate state of a Kleenean, neither true nor false.
    */
  case Maybe

  /**
    * The Boolean equivalent of a Kleenean.
    *
    * @param b either true or false.
    */
  case Truth(b: Boolean)

  /**
    * Apply method to convert this Kleenean to an Option[Boolean].
    *
    * @return None if this is Maybe, otherwise Some(b).
    */
  def apply(): Option[Boolean] = this match
    case Maybe => None
    case Truth(b) => Some(b)

  /**
    * Yield an Int corresponding to a Java-style comparison for this Kleenean.
    *
    * @return an Int which is either -1, 0 or 1.
    */
  def toInt: Int = this match
    case Maybe => 0
    case Truth(b) => if b then 1 else -1

  /**
    * Return the logical OR of this and k.
    *
    * @param k the other Kleenean (call-by-name, evaluated unless this is Truth(true)).
    * @return the logical OR.
    */
  def |(k: => Kleenean): Kleenean = this match
    case Truth(true) => this
    case Truth(false) => k
    case Maybe => k match
      case t@Truth(true) => t
      case _ => Maybe

  /**
    * Return the logical AND of this and k.
    *
    * @param k the other Kleenean (call-by-name, evaluated unless this is Truth(false)).
    * @return the logical AND.
    */
  def &(k: => Kleenean): Kleenean = this match
    case Truth(false) => this
    case Truth(true) => k
    case Maybe => k match
      case f@Truth(false) => f
      case _ => Maybe

  /**
    * Negate this Kleenean.
    *
    * @return the complement of this Kleenean.
    */
  def ! : Kleenean = this match
    case Maybe => Maybe
    case Truth(b) => Truth(!b)

  /**
    * Convert this Kleenean into a Boolean by providing a default value for the Maybe case.
    *
    * @param x the result if this is Maybe.
    * @return a Boolean corresponding to this Kleenean.
    */
  def getOrElse(x: => Boolean): Boolean = this match
    case Maybe => x
    case Truth(b) => b

  /**
    * @return "?" for Maybe, otherwise "T" or "F".
    */
  override def toString(): String = this match
    case Maybe => "?"
    case Truth(b) => if b then "T" else "F"

object Kleenean:
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
  def apply(x: Int): Kleenean = if x == 0 then Maybe else apply(x > 0)

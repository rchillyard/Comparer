/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

/**
  * A type alias representing a computation that produces a value of type `A`
  * when invoked. It essentially defines a no-argument function that returns `A`.
  *
  * This can be used for delaying computation or encapsulating side effects
  * that produce a value of type `A`.
  *
  * @tparam A the result type of the computation
  */
type Thunk[A] = () => A

/**
  * A type alias representing a function that takes an input of type `A`
  * and returns a boolean value. It is commonly used to define a condition
  * or a rule that evaluates to `true` or `false` based on the given input.
  *
  * @tparam A the type of the input to the predicate
  */
type Predicate[A] = A => Boolean

// The enum cases are used so often that they are available without their enum's prefix.
export Comparison.{Different, Same}
export Kleenean.{Maybe, Truth}

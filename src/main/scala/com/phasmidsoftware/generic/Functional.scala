/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmidsoftware.generic

import scala.language.postfixOps

/**
  * This is a collection of methods to manipulate functions.
  * NOTE: these methods are not available in the Scala library.
  *
  * @author scalaprof
  */
object Functional {

  /**
    * This method inverts the order of the first two parameters of a two-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam R  the result type
    * @return a curried function which takes the second parameter first
    */
  def invert2[T1, T2, R](f: T1 => T2 => R): T2 => T1 => R = t2 => t1 => f(t1)(t2)

  /**
    * This method inverts the order of the parameters of a two-parameter tupled function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam R  the result type
    * @return a tupled function which takes the second parameter first
    */
  def invert2[T1, T2, R](f: (T1, T2) => R): (T2, T1) => R = (t2, t1) => f(t1, t2)

  /**
    * This method inverts the order of the first three parameters of a three-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam R  the result type
    * @return a curried function which takes the third parameter first, then the second, etc.
    */
  def invert3[T1, T2, T3, R](f: T1 => T2 => T3 => R): T3 => T2 => T1 => R = t3 => t2 => t1 => f(t1)(t2)(t3)

  /**
    * This method inverts the order of the parameters of a three-parameter tupled function.
    *
    * NOTE: for inversion of greater numbers of parameters, please use tupleN(invertN(f))
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam R  the result type
    * @return a tupled function which takes the parameters in reverse order
    */
  def invert3[T1, T2, T3, R](f: (T1, T2, T3) => R): (T3, T2, T1) => R = (t3, t2, t1) => f(t1, t2, t3)

  /**
    * This method inverts the order of the first four parameters of a four-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam R  the result type
    * @return a curried function which takes the fourth parameter first, then the third, etc.
    */
  def invert4[T1, T2, T3, T4, R](f: T1 => T2 => T3 => T4 => R): T4 => T3 => T2 => T1 => R = t4 => t3 => t2 => t1 => f(t1)(t2)(t3)(t4)

  /**
    * This method inverts the order of the first four parameters of a four-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam T5 the type of the fifth parameter
    * @tparam R  the result type
    * @return a curried function which takes the fourth parameter first, then the third, etc.
    */
  def invert5[T1, T2, T3, T4, T5, R](f: T1 => T2 => T3 => T4 => T5 => R): T5 => T4 => T3 => T2 => T1 => R = t5 => t4 => t3 => t2 => t1 => f(t1)(t2)(t3)(t4)(t5)

  /**
    * This method inverts the order of the first four parameters of a four-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam T5 the type of the fifth parameter
    * @tparam T6 the type of the sixth parameter
    * @tparam R  the result type
    * @return a curried function which takes the fourth parameter first, then the third, etc.
    */
  def invert6[T1, T2, T3, T4, T5, T6, R](f: T1 => T2 => T3 => T4 => T5 => T6 => R): T6 => T5 => T4 => T3 => T2 => T1 => R = t6 => t5 => t4 => t3 => t2 => t1 => f(t1)(t2)(t3)(t4)(t5)(t6)

  /**
    * This method inverts the order of the first four parameters of a four-(or more-)parameter curried function.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam T5 the type of the fifth parameter
    * @tparam T6 the type of the sixth parameter
    * @tparam R  the result type
    * @return a curried function which takes the fourth parameter first, then the third, etc.
    */
  def invert7[T1, T2, T3, T4, T5, T6, T7, R](f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => R): T7 => T6 => T5 => T4 => T3 => T2 => T1 => R = t7 => t6 => t5 => t4 => t3 => t2 => t1 => f(t1)(t2)(t3)(t4)(t5)(t6)(t7)

  /**
    * This method uncurries the first two parameters of a two- (or more-) parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first two parameters of f;
    * whose second parameter is the third parameter, etc.
    *
    * This method is similar to f.tupled in the Scala library but that method will tuple all parameters of a curried function.
    * Here, we make it explicit how many parameters we wish to tuple.
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2)=>T4=>R
    */
  def tuple2[T1, T2, R](f: T1 => T2 => R): (T1, T2) => R = (t1, t2) => f(t1)(t2)

  /**
    * This method uncurries the first three parameters of a three- (or more-) parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first three parameters of f;
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2)=>T4=>R
    */
  def tuple3[T1, T2, T3, R](f: T1 => T2 => T3 => R): (T1, T2, T3) => R = (t1, t2, t3) => f(t1)(t2)(t3)

  /**
    * This method uncurries the first four parameters of a four- (or more-) parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first four parameters of f;
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2,T3,T4)=>R
    */
  def tuple4[T1, T2, T3, T4, R](f: T1 => T2 => T3 => T4 => R): (T1, T2, T3, T4) => R = (t1, t2, t3, t4) => f(t1)(t2)(t3)(t4)

  /**
    * This method uncurries the first five parameters of a five- (or more-) parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first five parameters of f;
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam T5 the type of the fifth parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2,T3,T4,T5)=>R
    */
  def tuple5[T1, T2, T3, T4, T5, R](f: T1 => T2 => T3 => T4 => T5 => R): (T1, T2, T3, T4, T5) => R = (t1, t2, t3, t4, t5) => f(t1)(t2)(t3)(t4)(t5)

  /**
    * This method uncurries the first six parameters of a six- (or more-) parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first six parameters of f;
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam T5 the type of the fifth parameter
    * @tparam T6 the type of the fifth parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2,T3,T4,T5,T6)=>R
    */
  def tuple6[T1, T2, T3, T4, T5, T6, R](f: T1 => T2 => T3 => T4 => T5 => T6 => R): (T1, T2, T3, T4, T5, T6) => R = (t1, t2, t3, t4, t5, t6) => f(t1)(t2)(t3)(t4)(t5)(t6)

  /**
    * This method uncurries the first seven parameters of a seven- (or more-) parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first seven parameters of f;
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam T5 the type of the fifth parameter
    * @tparam T6 the type of the sixth parameter
    * @tparam T7 the type of the seventh parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2,T3,T4,T5,T6,T7,T8)=>R
    */
  def tuple7[T1, T2, T3, T4, T5, T6, T7, R](f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => R): (T1, T2, T3, T4, T5, T6, T7) => R = (t1, t2, t3, t4, t5, t6, t7) => f(t1)(t2)(t3)(t4)(t5)(t6)(t7)

  /**
    * This method uncurries the first eight parameters of a eight- (or more-) parameter curried function.
    * The result is a (curried) function whose first parameter is a tuple of the first eight parameters of f;
    *
    * @param f the function
    * @tparam T1 the type of the first parameter
    * @tparam T2 the type of the second parameter
    * @tparam T3 the type of the third parameter
    * @tparam T4 the type of the fourth parameter
    * @tparam T5 the type of the fifth parameter
    * @tparam T6 the type of the sixth parameter
    * @tparam T7 the type of the seventh parameter
    * @tparam T8 the type of the eighth parameter
    * @tparam R  the result type of function f
    * @return a (curried) function of type (T1,T2,T3,T4,T5,T6,T7,T8)=>R
    */
  def tuple8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => R): (T1, T2, T3, T4, T5, T6, T7, T8) => R = (t1, t2, t3, t4, t5, t6, t7, t8) => f(t1)(t2)(t3)(t4)(t5)(t6)(t7)(t8)

  /**
    * This method (and the following similar methods) are missing from Function (or at least, the method in Function is flaky)
    *
    * @param f the function to be untupled
    * @tparam T1 the first parameter type
    * @tparam T2 the second parameter type
    * @tparam R  the result type
    * @return a function which takes the parameters separately (as part of a parameter set)
    */
  def untupled[T1, T2, R](f: ((T1, T2)) => R): (T1, T2) => R = {
    (x1, x2) => f(Tuple2(x1, x2))
  }

  /**
    * This method (and the following similar methods) are missing from Function (or at least, the method in Function is flaky)
    *
    * @param f the function to be untupled
    * @tparam T1 the first parameter type
    * @tparam T2 the second parameter type
    * @tparam T3 the third parameter type
    * @tparam R  the result type
    * @return a function which takes the parameters separately (as part of a parameter set)
    */
  def untupled[T1, T2, T3, R](f: ((T1, T2, T3)) => R): (T1, T2, T3) => R = {
    (x1, x2, x3) => f(Tuple3(x1, x2, x3))
  }

  /**
    * This method (and the following similar methods) are missing from Function (or at least, the method in Function is flaky)
    *
    * @param f the function to be untupled
    * @tparam T1 the first parameter type
    * @tparam T2 the second parameter type
    * @tparam T3 the third parameter type
    * @tparam T4 the fourth parameter type
    * @tparam R  the result type
    * @return a function which takes the parameters separately (as part of a parameter set)
    */
  def untupled[T1, T2, T3, T4, R](f: ((T1, T2, T3, T4)) => R): (T1, T2, T3, T4) => R = {
    (x1, x2, x3, x4) => f(Tuple4(x1, x2, x3, x4))
  }

  /**
    * This method (and the following similar methods) are missing from Function.
    *
    * @param f the function to be untupled
    * @tparam T1 the first parameter type
    * @tparam T2 the second parameter type
    * @tparam T3 the third parameter type
    * @tparam T4 the fourth parameter type
    * @tparam T5 the fifth parameter type
    * @tparam R  the result type
    * @return a function which takes the parameters separately (as part of a parameter set)
    */
  def untupled[T1, T2, T3, T4, T5, R](f: ((T1, T2, T3, T4, T5)) => R): (T1, T2, T3, T4, T5) => R = {
    (x1, x2, x3, x4, x5) => f(Tuple5(x1, x2, x3, x4, x5))
  }

  /**
    * This method (and the following similar methods) are missing from Function.
    *
    * @param f the function to be untupled
    * @tparam T1 the first parameter type
    * @tparam T2 the second parameter type
    * @tparam T3 the third parameter type
    * @tparam T4 the fourth parameter type
    * @tparam T5 the fifth parameter type
    * @tparam T6 the sixth parameter type
    * @tparam R  the result type
    * @return a function which takes the parameters separately (as part of a parameter set)
    */
  def untupled[T1, T2, T3, T4, T5, T6, R](f: ((T1, T2, T3, T4, T5, T6)) => R): (T1, T2, T3, T4, T5, T6) => R = {
    (x1, x2, x3, x4, x5, x6) => f(Tuple6(x1, x2, x3, x4, x5, x6))
  }

  /**
    * This method (and the following similar methods) are missing from Function.
    *
    * @param f the function to be untupled
    * @tparam T1 the first parameter type
    * @tparam T2 the second parameter type
    * @tparam T3 the third parameter type
    * @tparam T4 the fourth parameter type
    * @tparam T5 the fifth parameter type
    * @tparam T6 the sixth parameter type
    * @tparam T7 the seventh parameter type
    * @tparam R  the result type
    * @return a function which takes the parameters separately (as part of a parameter set)
    */
  def untupled[T1, T2, T3, T4, T5, T6, T7, R](f: ((T1, T2, T3, T4, T5, T6, T7)) => R): (T1, T2, T3, T4, T5, T6, T7) => R = {
    (x1, x2, x3, x4, x5, x6, x7) => f(Tuple7(x1, x2, x3, x4, x5, x6, x7))
  }
}
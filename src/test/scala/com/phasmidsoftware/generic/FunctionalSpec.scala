/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.generic

import com.phasmidsoftware.generic.Functional.*
import org.scalatest.*

/**
  *
  * @author scalaprof
  */
class FunctionalSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers:
  behavior of "invert2"
  it should "work" in:
    val f: Int => Int => String = { a => b => "abcde".substring(a, b) }
    f(1)(2) shouldBe "b"
    val g: Int => Int => String = invert2(f)
    g(2)(1) shouldBe "b"

  behavior of "invert2 (tupled)"
  it should "work" in:
    def f(a: Int, b: String): String = b * a

    f(2, "a") shouldBe "aa"
    val g = invert2(f)
    g("a", 2) shouldBe "aa"

  behavior of "invert3 (tupled)"
  it should "work" in:
    def combine(x: Int, y: String, z: Boolean): String = if z then y * x else (y.toInt + x).toString

    combine(2, "a", z = true) shouldBe "aa"
    combine(2, "3", z = false) shouldBe "5"
    val g = invert3(combine)
    g(true, "a", 2) shouldBe "aa"
    g(false, "3", 2) shouldBe "5"

  behavior of "invert3 (curried)"
  it should "work" in:
    val f: Int => Int => Int => Int = { a => b => c => a * b + c }
    f(2)(3)(4) shouldBe 10
    val aux = invert3(f)
    aux(2)(3)(4) shouldBe 14

  behavior of "invert4"
  it should "work" in:
    val f: Int => Int => Int => Int => Int = { a => b => c => d => a * b + c % d }
    f(2)(3)(4)(5) shouldBe 10
    invert4(f)(2)(3)(4)(5) shouldBe 21

  behavior of "invert5"
  it should "work" in:
    val f: Int => Int => Int => Int => Int => String = { a => b => c => d => e => s"$a$b$c$d$e" }
    f(1)(2)(3)(4)(5) shouldBe "12345"
    invert5(f)(1)(2)(3)(4)(5) shouldBe "54321"

  behavior of "invert6"
  it should "work" in:
    val f: Int => Int => Int => Int => Int => Int => String = { a => b => c => d => e => g => s"$a$b$c$d$e$g" }
    f(1)(2)(3)(4)(5)(6) shouldBe "123456"
    invert6(f)(1)(2)(3)(4)(5)(6) shouldBe "654321"

  behavior of "invert7"
  it should "work" in:
    val f: Int => Int => Int => Int => Int => Int => Int => String = { a => b => c => d => e => g => h => s"$a$b$c$d$e$g$h" }
    f(1)(2)(3)(4)(5)(6)(7) shouldBe "1234567"
    invert7(f)(1)(2)(3)(4)(5)(6)(7) shouldBe "7654321"

  behavior of "tupled2"
  it should "work for a 2-stage function" in:
    def f: Int => Int => Int = { a => b => a * b }

    f(2)(3) shouldBe 6

    def g = tuple2(f)

    g(2, 3) shouldBe 6
  it should "work for a 3-stage function" in:
    def f: Int => Int => Int => Int = { a => b => c => a * b + c }

    f(2)(3)(4) shouldBe 10

    def g = tuple2(f)

    g(2, 3)(4) shouldBe 10

  behavior of "tuple3"
  it should "work" in:
    def f: Int => Int => Int => Int = { a => b => c => a * b + c }

    f(2)(3)(4) shouldBe 10

    def g = tuple3(f)

    g(2, 3, 4) shouldBe 10

  behavior of "tuple4"
  it should "work" in:
    val f: Int => Int => Int => Int => String = { a => b => c => d => s"$a$b$c$d" }
    tuple4(f)(1, 2, 3, 4) shouldBe "1234"

  behavior of "tuple5"
  it should "work" in:
    val f: Int => Int => Int => Int => Int => String = { a => b => c => d => e => s"$a$b$c$d$e" }
    tuple5(f)(1, 2, 3, 4, 5) shouldBe "12345"

  behavior of "tuple6"
  it should "work" in:
    val f: Int => Int => Int => Int => Int => Int => String = { a => b => c => d => e => g => s"$a$b$c$d$e$g" }
    tuple6(f)(1, 2, 3, 4, 5, 6) shouldBe "123456"

  behavior of "tuple7"
  it should "work" in:
    val f: Int => Int => Int => Int => Int => Int => Int => String = { a => b => c => d => e => g => h => s"$a$b$c$d$e$g$h" }
    tuple7(f)(1, 2, 3, 4, 5, 6, 7) shouldBe "1234567"

  behavior of "tuple8"
  it should "work" in:
    val f: Int => Int => Int => Int => Int => Int => Int => Int => String = { a => b => c => d => e => g => h => i => s"$a$b$c$d$e$g$h$i" }
    tuple8(f)(1, 2, 3, 4, 5, 6, 7, 8) shouldBe "12345678"

  behavior of "untupled"
  it should "work for 2 parameters" in:
    val f: ((Int, Int)) => Int = { case (a, b) => a * 10 + b }
    untupled(f)(1, 2) shouldBe 12
  it should "work for 3 parameters" in:
    val f: ((Int, Int, Int)) => Int = { case (a, b, c) => a * 100 + b * 10 + c }
    untupled(f)(1, 2, 3) shouldBe 123
  it should "work for 4 parameters" in:
    val f: ((Int, Int, Int, Int)) => String = { case (a, b, c, d) => s"$a$b$c$d" }
    untupled(f)(1, 2, 3, 4) shouldBe "1234"
  it should "work for 5 parameters" in:
    val f: ((Int, Int, Int, Int, Int)) => String = { case (a, b, c, d, e) => s"$a$b$c$d$e" }
    untupled(f)(1, 2, 3, 4, 5) shouldBe "12345"
  it should "work for 6 parameters" in:
    val f: ((Int, Int, Int, Int, Int, Int)) => String = { case (a, b, c, d, e, g) => s"$a$b$c$d$e$g" }
    untupled(f)(1, 2, 3, 4, 5, 6) shouldBe "123456"
  it should "work for 7 parameters" in:
    val f: ((Int, Int, Int, Int, Int, Int, Int)) => String = { case (a, b, c, d, e, g, h) => s"$a$b$c$d$e$g$h" }
    untupled(f)(1, 2, 3, 4, 5, 6, 7) shouldBe "1234567"

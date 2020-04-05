package com.phasmidsoftware.generic

import com.phasmidsoftware.generic.Functional._
import org.scalatest._

/**
  *
  * @author scalaprof
  */
class FunctionalSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers {
  behavior of "invert2"
  it should "work" in {
    val f: Int => Int => String = { a => b => "abcde".substring(a, b) }
    f(1)(2) shouldBe "b"
    val g: Int => Int => String = invert2(f)
    g(2)(1) shouldBe "b"
  }

  behavior of "invert3 (tupled)"
  it should "work" in {
    def combine(x: Int, y: String, z: Boolean): String = if (z) y * x else (y.toInt + x).toString

    combine(2, "a", z = true) shouldBe "aa"
    combine(2, "3", z = false) shouldBe "5"
    val g = invert3(combine _)
    g(true, "a", 2) shouldBe "aa"
    g(false, "3", 2) shouldBe "5"
  }

  behavior of "invert3 (curried)"
  it should "work" in {
    val f: Int => Int => Int => Int = { a => b => c => a * b + c }
    f(2)(3)(4) shouldBe 10
    val aux = invert3(f)
    aux(2)(3)(4) shouldBe 14
  }

  behavior of "invert4"
  it should "work" in {
    val f: Int => Int => Int => Int => Int = { a => b => c => d => a * b + c % d }
    f(2)(3)(4)(5) shouldBe 10
    invert4(f)(2)(3)(4)(5) shouldBe 21
  }

  behavior of "tupled2"
  it should "work for a 2-stage function" in {
    def f: Int => Int => Int = { a => b => a * b }

    f(2)(3) shouldBe 6

    def g = tuple2(f)

    g(2, 3) shouldBe 6
  }
  it should "work for a 3-stage function" in {
    def f: Int => Int => Int => Int = { a => b => c => a * b + c }

    f(2)(3)(4) shouldBe 10

    def g = tuple2(f)

    g(2, 3)(4) shouldBe 10
  }

  behavior of "tuple3"
  it should "work" in {
    def f: Int => Int => Int => Int = { a => b => c => a * b + c }

    f(2)(3)(4) shouldBe 10

    def g = tuple3(f)

    g(2, 3, 4) shouldBe 10
  }

}

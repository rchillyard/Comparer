/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import org.scalatest.{FlatSpec, Matchers}

class KleeneanSpec extends FlatSpec with Matchers {

  behavior of "Kleenean"

  it should "toInt" in {
    Maybe.toInt shouldBe 0
    Truth(true).toInt shouldBe 1
    Truth(false).toInt shouldBe -1
  }

  it should "getOrElse" in {
    Maybe.getOrElse(true) shouldBe true
    Maybe.getOrElse(false) shouldBe false
    Truth(true).getOrElse(true) shouldBe true
    Truth(false).getOrElse(true) shouldBe false
    Truth(true).getOrElse(false) shouldBe true
    Truth(false).getOrElse(false) shouldBe false
  }

  it should "apply()" in {
    Maybe() shouldBe None
    Truth(true)() shouldBe Some(true)
    Truth(false)() shouldBe Some(false)
  }

  it should "apply" in {
    Kleenean(0) shouldBe Maybe
    Kleenean(-1) shouldBe Truth(false)
    Kleenean(1) shouldBe Truth(true)
  }

  it should "$amp" in {
    Truth(true) & Truth(true) shouldBe Truth(true)
    Truth(true) & Truth(false) shouldBe Truth(false)
    Truth(false) & Truth(true) shouldBe Truth(false)
    Truth(true) & Maybe shouldBe Maybe
    Maybe & Truth(true) shouldBe Maybe
    Maybe & Maybe shouldBe Maybe
    Truth(false) & Maybe shouldBe Truth(false)
    Maybe & Truth(false) shouldBe Truth(false)
    Truth(false) & Truth(false) shouldBe Truth(false)
  }

  it should "$bar" in {
    Truth(true) | Truth(true) shouldBe Truth(true)
    Truth(true) | Truth(false) shouldBe Truth(true)
    Truth(false) | Truth(true) shouldBe Truth(true)
    Truth(true) | Maybe shouldBe Truth(true)
    Maybe | Truth(true) shouldBe Truth(true)
    Maybe | Maybe shouldBe Maybe
    Truth(false) | Maybe shouldBe Maybe
    Maybe | Truth(false) shouldBe Maybe
    Truth(false) | Truth(false) shouldBe Truth(false)

  }

}

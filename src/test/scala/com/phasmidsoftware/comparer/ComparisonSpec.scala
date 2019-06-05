/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import com.phasmidsoftware.comparer.Comparison.{Less, More}
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class ComparisonSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "Comparison"

  it should "apply(Int)" in {
    Comparison(0) shouldBe Same
    Comparison(1) shouldBe More
    Comparison(-1) shouldBe Less
  }
  it should "toInt" in {
    Comparison(-1).toInt shouldBe -1
    Comparison(0).toInt shouldBe 0
    Comparison(1).toInt shouldBe 1
  }
  it should "apply(Option[Boolean])" in {
    Comparison(None) shouldBe Same
    Comparison(Some(false)) shouldBe More
    Comparison(Some(true)) shouldBe Less
  }
  it should "flip" in {
    More.flip shouldBe Less
    Less.flip shouldBe More
    Same.flip shouldBe Same
  }
  it should "orElse" in {
    More orElse More shouldBe More
    More orElse Less shouldBe More
    Less orElse More shouldBe Less
    Less orElse Less shouldBe Less
    Same orElse Less shouldBe Less
    Same orElse More shouldBe More
    Same orElse Same shouldBe Same
  }
  it should "implement | correctly" in {
    More | Same shouldBe More
    Same | More shouldBe More
    Less | Same shouldBe Same
    Same | Less shouldBe Same
    More | More shouldBe More
    More | Less shouldBe More
    Less | More shouldBe More
    Same | Same shouldBe Same
    Less | Less shouldBe Less
  }
  it should "implement & correctly" in {
    More & Same shouldBe Same
    Same & More shouldBe Same
    Less & Same shouldBe Less
    Same & Less shouldBe Less
    More & More shouldBe More
    More & Less shouldBe Less
    Less & More shouldBe Less
    Same & Same shouldBe Same
    Less & Less shouldBe Less
  }

  it should "implement || correctly" in {
    More || Same shouldBe More
    Same || More shouldBe More
    Less || Same shouldBe Same
    Same || Less shouldBe Same
    More || More shouldBe More
    More || Less shouldBe More
    Less || More shouldBe More
    Same || Same shouldBe Same
    Less || Less shouldBe Less
  }
  it should "implement && correctly" in {
    More && Same shouldBe Same
    Same && More shouldBe Same
    Less && Same shouldBe Less
    Same && Less shouldBe Less
    More && More shouldBe More
    More && Less shouldBe Less
    Less && More shouldBe Less
    Same && Same shouldBe Same
    Less && Less shouldBe Less
  }
}
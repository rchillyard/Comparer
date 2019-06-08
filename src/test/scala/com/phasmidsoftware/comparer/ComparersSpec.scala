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
class ComparersSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  private val c1a = Composite(1, "a")
  private val c2a = Composite(2, "a")
  private val c1z = Composite(1, "z")

  case class DateJ(year: Int, month: Int, day: Int)

  object MyComparers extends Comparers {
    val dateComparer: Comparer[DateJ] = comparer3(DateJ)
    val compositeComparer: Comparer[Composite] = comparer2(Composite.apply)
  }

  behavior of "comparers"

  it should "compare" in {
    val today = DateJ(2019, 6, 5)
    val tomorrow = DateJ(2019, 6, 6)
    val yesterday = DateJ(2019, 6, 4)
    val nextMonth = DateJ(2019, 7, 5)
    val lastMonth = DateJ(2019, 5, 5)
    val nextYear = DateJ(2020, 6, 5)
    val lastYear = DateJ(2018, 6, 5)
    import MyComparers._
    dateComparer(today)(today) shouldBe Same
    dateComparer(today)(tomorrow) shouldBe More
    dateComparer(tomorrow)(today) shouldBe Less
    dateComparer(today)(yesterday) shouldBe Less
    dateComparer(today)(nextMonth) shouldBe More
    dateComparer(today)(lastMonth) shouldBe Less
    dateComparer(today)(nextYear) shouldBe More
    dateComparer(today)(lastYear) shouldBe Less
  }

  it should "compare Composite" in {
    import MyComparers._
    compositeComparer(c1z)(c1a) shouldBe Less
    compositeComparer(c2a)(c1a) shouldBe Less
    compositeComparer(c2a)(c1z) shouldBe Less
    compositeComparer(c1a)(c1a) shouldBe Same
    compositeComparer(c1a)(c2a) shouldBe More
    compositeComparer(c1a)(c1z) shouldBe More
  }

}


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

  behavior of "comparers"

  it should "compare opt" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[Option[Int]] = comparerOpt
    }
    import MyComparers._
    comparer(Some(3))(Some(2)) shouldBe Less
    comparer(Some(1))(None) shouldBe Same
    comparer(None)(Some(1)) shouldBe Same
    comparer(Some(2))(Some(3)) shouldBe More
  }

  it should "compare sequence" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[Seq[Int]] = comparerSeq
    }
    import MyComparers._
    comparer(Seq(3))(Seq(2)) shouldBe Less
    comparer(Seq(1))(Nil) shouldBe Same
    comparer(Nil)(Seq(1)) shouldBe Same
    comparer(Seq(2))(Seq(3)) shouldBe More
  }

  it should "compare 2" in {
    val c1a = Composite(1, "a")
    val c2a = Composite(2, "a")
    val c1z = Composite(1, "z")
    object MyComparers extends Comparers {
      val comparer: Comparer[Composite] = comparer2(Composite.apply)
    }
    import MyComparers._
    comparer(c1z)(c1a) shouldBe Less
    comparer(c2a)(c1a) shouldBe Less
    comparer(c2a)(c1z) shouldBe Less
    comparer(c1a)(c1a) shouldBe Same
    comparer(c1a)(c2a) shouldBe More
    comparer(c1a)(c1z) shouldBe More
  }

  it should "compare 3" in {
    case class DateJ(year: Int, month: Int, day: Int)
    val today = DateJ(2019, 6, 5)
    val tomorrow = DateJ(2019, 6, 6)
    val yesterday = DateJ(2019, 6, 4)
    val nextMonth = DateJ(2019, 7, 5)
    val lastMonth = DateJ(2019, 5, 5)
    val nextYear = DateJ(2020, 6, 5)
    val lastYear = DateJ(2018, 6, 5)
    object MyComparers extends Comparers {
      val comparer: Comparer[DateJ] = comparer3(DateJ)
    }
    import MyComparers._
    comparer(today)(today) shouldBe Same
    comparer(today)(tomorrow) shouldBe More
    comparer(tomorrow)(today) shouldBe Less
    comparer(today)(yesterday) shouldBe Less
    comparer(today)(nextMonth) shouldBe More
    comparer(today)(lastMonth) shouldBe Less
    comparer(today)(nextYear) shouldBe More
    comparer(today)(lastYear) shouldBe Less
  }

  it should "compare 4" in {
    case class Case4(x1: Int, x2: Int, x3: Int, x4: Int)
    object MyComparers extends Comparers {
      val comparer: Comparer[Case4] = comparer4(Case4)
    }
    import MyComparers._
    comparer(Case4(1, 2, 3, 4))(Case4(2, 1, 3, 4)) shouldBe More
    comparer(Case4(1, 2, 3, 4))(Case4(1, 2, 3, 5)) shouldBe More
  }

  it should "compare 5" in {
    case class Case5(x1: Int, x2: Int, x3: Int, x4: Int, x5: Int)
    object MyComparers extends Comparers {
      val comparer: Comparer[Case5] = comparer5(Case5)
    }
    import MyComparers._
    comparer(Case5(1, 2, 3, 4, 0))(Case5(2, 1, 3, 4, 0)) shouldBe More
    comparer(Case5(1, 2, 3, 4, 0))(Case5(1, 2, 3, 4, 1)) shouldBe More
  }

  it should "compare 6" in {
    case class Case6(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long)
    object MyComparers extends Comparers {
      val comparer: Comparer[Case6] = comparer6(Case6)
    }
    import MyComparers._
    comparer(Case6(1, 2, "3", Some(4), 0, 99L))(Case6(2, 1, "3", Some(4), 0, 99L)) shouldBe More
    comparer(Case6(1, 2, "3", None, 0, 99L))(Case6(1, 2, "3", Some(4), 1, 100L)) shouldBe More
  }
}

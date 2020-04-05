/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import com.phasmidsoftware.comparer.Comparison.{Less, More}
import com.phasmidsoftware.generic.Functional
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{flatspec, matchers}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
  * @author scalaprof
  */
class ComparersSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers with Futures with ScalaFutures {

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

  it should "compare iterable" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[Iterable[Int]] = comparerIterable
    }
    import MyComparers._
    comparer(Seq(3))(Seq(2)) shouldBe Less
    comparer(Seq(1))(Nil) shouldBe Same
    comparer(Nil)(Seq(1)) shouldBe Same
    comparer(Seq(2))(Seq(3)) shouldBe More
  }

  it should "compare iterable 2" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[Iterable[Int]] = comparerIterable
    }
    import MyComparers._
    comparer(Seq(3))(Seq(2, 3)) shouldBe Less
    comparer(Seq(1, 2))(Nil) shouldBe Same
    comparer(Seq(1, 2))(Seq(1)) shouldBe Same
    comparer(Seq(2, 1))(Seq(3)) shouldBe More
  }

  it should "compare sequence" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[Seq[Int]] = comparerSeq
    }
    import MyComparers._
    comparer(List(3))(Seq(2, 3)) shouldBe Less
    comparer(Seq(1, 2))(Nil) shouldBe Same
    comparer(Seq(1, 2))(List(1)) shouldBe Same
    comparer(Seq(2, 1))(Seq(3)) shouldBe More
  }

  it should "compare list" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[List[Int]] = comparerList
    }
    import MyComparers._
    comparer(List(3))(List(2, 3)) shouldBe Less
    comparer(List(1, 2))(Nil) shouldBe Same
    comparer(List(1, 2))(List(1)) shouldBe Same
    comparer(List(2, 1))(List(3)) shouldBe More
  }

  it should "compare array" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[Array[Int]] = comparerArray
    }
    import MyComparers._
    comparer(Array(3))(Array(2, 3)) shouldBe Less
    comparer(Array(1, 2))(Array()) shouldBe Same
    comparer(Array(1, 2))(Array(1)) shouldBe Same
    comparer(Array(2, 1))(Array(3)) shouldBe More
  }

  it should "compare try" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[Try[Int]] = comparerTry
    }
    import MyComparers._
    comparer(Success(3))(Success(2)) shouldBe Less
    comparer(Success(1))(Failure(null)) shouldBe Same
    comparer(Failure(null))(Success(1)) shouldBe Same
    comparer(Success(2))(Success(3)) shouldBe More
  }

  it should "compare either" in {
    object MyComparers extends Comparers {
      val comparer: Comparer[Either[String, Int]] = comparerEither
    }
    import MyComparers._
    comparer(Right(3))(Right(2)) shouldBe Less
    comparer(Right(1))(Left("bad")) shouldBe Same
    comparer(Left(null))(Right(1)) shouldBe Same
    comparer(Right(2))(Right(3)) shouldBe More
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

  it should "compare 1" in {
    val o0 = Option(0)
    val o1 = Option(1)
    val o2 = Option(2)
    object MyComparers extends Comparers {
      val comparer: Comparer[Option[Int]] = comparer1 { x: Int => Option(x) }
    }
    import MyComparers._
    comparer(o0)(o1) shouldBe More
    comparer(o2)(o1) shouldBe Less
    comparer(o2)(o0) shouldBe Less
    comparer(o1)(o1) shouldBe Same
    comparer(o1)(o2) shouldBe More
    comparer(o1)(o0) shouldBe Less
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

  it should "compare 3 in reverse order" in {
    case class DateJ(day: Int, month: Int, year: Int)
    object MyComparers extends Comparers {

      import Functional._

      val comparer: Comparer[DateJ] = comparer3(invert3(DateJ))
    }
    import MyComparers._
    val today = DateJ(5, 6, 2019)
    val tomorrow = DateJ(6, 6, 2019)
    val yesterday = DateJ(4, 6, 2019)
    val nextMonth = DateJ(5, 7, 2019)
    val lastMonth = DateJ(5, 5, 2019)
    val nextYear = DateJ(5, 6, 2020)
    val lastYear = DateJ(5, 6, 2018)
    comparer(today)(today) shouldBe Same
    comparer(today)(tomorrow) shouldBe More
    comparer(tomorrow)(today) shouldBe Less
    comparer(today)(yesterday) shouldBe Less
    comparer(today)(nextMonth) shouldBe More
    comparer(today)(lastMonth) shouldBe Less
    comparer(today)(nextYear) shouldBe More
    comparer(today)(lastYear) shouldBe Less
  }

  it should "compare 3 in reverse order using more general approach" in {
    case class DateJ(day: Int, month: Int, year: Int)
    val today = DateJ(5, 6, 2019)
    val tomorrow = DateJ(6, 6, 2019)
    val yesterday = DateJ(4, 6, 2019)
    val nextMonth = DateJ(5, 7, 2019)
    val lastMonth = DateJ(5, 5, 2019)
    val nextYear = DateJ(5, 6, 2020)
    val lastYear = DateJ(5, 6, 2018)
    object MyComparers extends Comparers {

      import Functional._

      val comparer: Comparer[DateJ] = comparer3(tuple3(invert3(DateJ.curried)))
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

  it should "compare 3 in mixed up order" in {
    case class DateJ(month: Int, day: Int, year: Int)
    val today = DateJ(6, 5, 2019)
    val tomorrow = DateJ(6, 6, 2019)
    val yesterday = DateJ(6, 4, 2019)
    val nextMonth = DateJ(7, 5, 2019)
    val lastMonth = DateJ(5, 5, 2019)
    val nextYear = DateJ(6, 5, 2020)
    val lastYear = DateJ(6, 5, 2018)
    object MyComparers extends Comparers {

      import Functional._

      val comparer: Comparer[DateJ] = comparer3(tuple3(invert2(DateJ.curried)))
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

  it should "compare 7" in {
    case class Case7(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Boolean)
    object MyComparers extends Comparers {
      val comparer: Comparer[Case7] = comparer7(Case7)
    }
    import MyComparers._
    comparer(Case7(1, 2, "3", Some(4), 0, 99L, x7 = true))(Case7(1, 2, "3", Some(5), 0, 99L, x7 = true)) shouldBe More
    comparer(Case7(1, 2, "3", None, 0, 99L, x7 = false))(Case7(1, 2, "3", Some(4), 1, 100L, x7 = false)) shouldBe More
  }

  it should "compare 8" in {
    case class Case8(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Either[String, Int], x8: Boolean)
    object MyComparers extends Comparers {
      val comparer: Comparer[Case8] = comparer8(Case8)
    }
    import MyComparers._
    comparer(Case8(1, 2, "3", Some(4), 0, 99L, Right(3), x8 = true))(Case8(1, 2, "3", Some(4), 0, 99L, Right(4), x8 = true)) shouldBe More
    comparer(Case8(1, 2, "3", None, 0, 99L, Left(""), x8 = false))(Case8(1, 2, "3", Some(4), 1, 100L, Left(""), x8 = false)) shouldBe More
  }

  it should "compare 9" in {
    case class Case9(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Either[String, Int], x8: List[Int], x9: Boolean)
    object MyComparers extends Comparers {
      val comparer: Comparer[Case9] = comparer9(Case9)
    }
    import MyComparers._
    comparer(Case9(1, 2, "3", Some(4), 0, 99L, Right(3), List(1), x9 = true))(Case9(1, 2, "3", Some(4), 0, 99L, Right(3), List(2, 1), x9 = true)) shouldBe More
    comparer(Case9(1, 2, "3", None, 0, 99L, Left(""), Nil, x9 = false))(Case9(1, 2, "3", Some(4), 1, 99L, Left(""), List(1), x9 = false)) shouldBe More
  }

  it should "compare 10" in {
    case class Case10(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Either[String, Int], x8: Iterable[Int], x9: Iterable[Int], x10: Boolean)
    object MyComparers extends Comparers {
      val comparer: Comparer[Case10] = comparer10(Case10)
    }
    import MyComparers._
    comparer(Case10(1, 2, "3", Some(4), 0, 1010L, Right(3), Seq(1), Seq(1), x10 = true))(Case10(1, 2, "3", Some(4), 0, 1010L, Right(3), Seq(2, 1), Seq(1), x10 = true)) shouldBe More
    comparer(Case10(1, 2, "3", None, 0, 1010L, Left(""), Nil, Array(1), x10 = false))(Case10(1, 2, "3", Some(4), 1, 1010L, Left(""), Seq(1), Array(1), x10 = false)) shouldBe More
  }

  it should "compare 11" in {
    case class Case11(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Either[String, Int], x8: Iterable[Int], x9: Iterable[Int], x10: Boolean, x11: Try[Int])
    object MyComparers extends Comparers {
      val comparer: Comparer[Case11] = comparer11(Case11)
    }
    import MyComparers._
    comparer(Case11(1, 2, "3", Some(4), 0, 1010L, Right(3), Seq(2, 1), Seq(1), x10 = true, Success(1)))(Case11(1, 2, "3", Some(4), 0, 1010L, Right(3), Seq(2, 1), Seq(1), x10 = true, Success(2))) shouldBe More
    comparer(Case11(1, 2, "3", None, 0, 1010L, Left(""), Nil, Array(1), x10 = false, Success(0)))(Case11(1, 2, "3", None, 0, 1010L, Left(""), Nil, Array(1), x10 = false, Success(1))) shouldBe More
  }
}


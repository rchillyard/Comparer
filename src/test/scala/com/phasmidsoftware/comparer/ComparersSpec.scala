/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import com.phasmidsoftware.comparer.Comparison.{Less, More}
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{flatspec, matchers}

import scala.util.{Failure, Success, Try}

/**
  * Specification of the given Comparers for standard types, and of derived Comparers.
  *
  * @author scalaprof
  */
class ComparersSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers with Futures with ScalaFutures:

  behavior of "given comparers"

  it should "compare opt" in:
    val comparer = summon[Comparer[Option[Int]]]
    comparer(Some(3))(Some(2)) shouldBe Less
    comparer(Some(1))(None) shouldBe Same
    comparer(None)(Some(1)) shouldBe Same
    comparer(Some(2))(Some(3)) shouldBe More

  it should "compare iterable" in:
    val comparer = summon[Comparer[Iterable[Int]]]
    comparer(Seq(3))(Seq(2)) shouldBe Less
    comparer(Seq(1))(Nil) shouldBe Same
    comparer(Nil)(Seq(1)) shouldBe Same
    comparer(Seq(2))(Seq(3)) shouldBe More

  it should "compare iterable 2" in:
    val comparer = summon[Comparer[Iterable[Int]]]
    comparer(Seq(3))(Seq(2, 3)) shouldBe Less
    comparer(Seq(1, 2))(Nil) shouldBe Same
    comparer(Seq(1, 2))(Seq(1)) shouldBe Same
    comparer(Seq(2, 1))(Seq(3)) shouldBe More

  it should "comparePrefixes" in:
    Comparer.comparePrefixes(Seq(3), Seq(2, 3)) shouldBe More
    Comparer.comparePrefixes(Seq(1, 2), Nil) shouldBe Same
    Comparer.comparePrefixes(Seq(1, 2), Seq(1)) shouldBe Same
    Comparer.comparePrefixes(Seq(2, 1), Seq(3)) shouldBe Less

  it should "compareAll" in:
    Comparer.compareAll(Seq(3), Seq(2, 3)) shouldBe More
    Comparer.compareAll(Seq(1, 2), Nil) shouldBe More
    Comparer.compareAll(Seq(1, 2), Seq(1)) shouldBe More
    Comparer.compareAll(Seq(2, 1), Seq(3)) shouldBe Less

  it should "compare sequence" in:
    val comparer = summon[Comparer[Seq[Int]]]
    comparer(List(3))(Seq(2, 3)) shouldBe Less
    comparer(Seq(1, 2))(Nil) shouldBe Same
    comparer(Seq(1, 2))(List(1)) shouldBe Same
    comparer(Seq(2, 1))(Seq(3)) shouldBe More

  it should "compare list" in:
    val comparer = summon[Comparer[List[Int]]]
    comparer(List(3))(List(2, 3)) shouldBe Less
    comparer(List(1, 2))(Nil) shouldBe Same
    comparer(List(1, 2))(List(1)) shouldBe Same
    comparer(List(2, 1))(List(3)) shouldBe More

  it should "compare array" in:
    val comparer = summon[Comparer[Array[Int]]]
    comparer(Array(3))(Array(2, 3)) shouldBe Less
    comparer(Array(1, 2))(Array()) shouldBe Same
    comparer(Array(1, 2))(Array(1)) shouldBe Same
    comparer(Array(2, 1))(Array(3)) shouldBe More

  it should "compare try" in:
    val comparer = summon[Comparer[Try[Int]]]
    comparer(Success(3))(Success(2)) shouldBe Less
    comparer(Success(1))(Failure(null)) shouldBe Same
    comparer(Failure(null))(Success(1)) shouldBe Same
    comparer(Success(2))(Success(3)) shouldBe More

  it should "compare either" in:
    val comparer = summon[Comparer[Either[String, Int]]]
    comparer(Right(3))(Right(2)) shouldBe Less
    comparer(Right(1))(Left("bad")) shouldBe Same
    comparer(Left(null))(Right(1)) shouldBe Same
    comparer(Right(2))(Right(3)) shouldBe More

  behavior of "derived comparers"

  it should "derive a comparer for a case class with one field" in:
    case class Wrapper(x: Int) derives Comparer
    val comparer = summon[Comparer[Wrapper]]
    val w0 = Wrapper(0)
    val w1 = Wrapper(1)
    val w2 = Wrapper(2)
    comparer(w0)(w1) shouldBe More
    comparer(w2)(w1) shouldBe Less
    comparer(w2)(w0) shouldBe Less
    comparer(w1)(w1) shouldBe Same
    comparer(w1)(w2) shouldBe More
    comparer(w1)(w0) shouldBe Less

  it should "derive a comparer for a case class with two fields" in:
    case class Pair(i: Int, s: String) derives Comparer
    val comparer = summon[Comparer[Pair]]
    val c1a = Pair(1, "a")
    val c2a = Pair(2, "a")
    val c1z = Pair(1, "z")
    comparer(c1z)(c1a) shouldBe Less
    comparer(c2a)(c1a) shouldBe Less
    comparer(c2a)(c1z) shouldBe Less
    comparer(c1a)(c1a) shouldBe Same
    comparer(c1a)(c2a) shouldBe More
    comparer(c1a)(c1z) shouldBe More

  it should "derive a comparer for a case class with two fields (tupled)" in:
    case class Pair(i: Int, s: String) derives Comparer
    val comparer = summon[Comparer[Pair]]
    val c1a = Pair(1, "a")
    val c2a = Pair(2, "a")
    val c1z = Pair(1, "z")
    comparer.compare(c1a, c1z) shouldBe Less
    comparer.compare(c1a, c2a) shouldBe Less
    comparer.compare(c1z, c2a) shouldBe Less
    comparer.compare(c1a, c1a) shouldBe Same
    comparer.compare(c2a, c1a) shouldBe More
    comparer.compare(c1z, c1a) shouldBe More

  it should "derive a comparer for a case class with a tuple field" in:
    case class Strange(x: Int, y: (String, Int)) derives Comparer
    val comparer = summon[Comparer[Strange]]
    val c1a = Strange(1, "a" -> 1)
    val c2a = Strange(2, "a" -> 1)
    val c1z = Strange(1, "z" -> 1)
    comparer(c1z)(c1a) shouldBe Less
    comparer(c2a)(c1a) shouldBe Less
    comparer(c2a)(c1z) shouldBe Less
    comparer(c1a)(c1a) shouldBe Same
    comparer(c1a)(c2a) shouldBe More
    comparer(c1a)(c1z) shouldBe More

  it should "derive a comparer for a case class with three fields" in:
    case class DateJ(year: Int, month: Int, day: Int) derives Comparer
    val comparer = summon[Comparer[DateJ]]
    val today = DateJ(2019, 6, 5)
    val tomorrow = DateJ(2019, 6, 6)
    val yesterday = DateJ(2019, 6, 4)
    val nextMonth = DateJ(2019, 7, 5)
    val lastMonth = DateJ(2019, 5, 5)
    val nextYear = DateJ(2020, 6, 5)
    val lastYear = DateJ(2018, 6, 5)
    comparer(today)(today) shouldBe Same
    comparer(today)(tomorrow) shouldBe More
    comparer(tomorrow)(today) shouldBe Less
    comparer(today)(yesterday) shouldBe Less
    comparer(today)(nextMonth) shouldBe More
    comparer(today)(lastMonth) shouldBe Less
    comparer(today)(nextYear) shouldBe More
    comparer(today)(lastYear) shouldBe Less

  behavior of "explicit field order"

  // A derived Comparer always compares the fields in the order they are declared.
  // For any other order, build the Comparer from the fields themselves.

  it should "compare the fields of a case class in reverse order" in:
    case class DateJ(day: Int, month: Int, year: Int)
    val comparer = Comparer.same[DateJ] :| (_.year) :| (_.month) :| (_.day)
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

  it should "compare the fields of a case class in mixed up order" in:
    case class DateJ(month: Int, day: Int, year: Int)
    val comparer = Comparer[DateJ, Int](_.year, _.month, _.day)
    val today = DateJ(6, 5, 2019)
    val tomorrow = DateJ(6, 6, 2019)
    val yesterday = DateJ(6, 4, 2019)
    val nextMonth = DateJ(7, 5, 2019)
    val lastMonth = DateJ(5, 5, 2019)
    val nextYear = DateJ(6, 5, 2020)
    val lastYear = DateJ(6, 5, 2018)
    comparer(today)(today) shouldBe Same
    comparer(today)(tomorrow) shouldBe More
    comparer(tomorrow)(today) shouldBe Less
    comparer(today)(yesterday) shouldBe Less
    comparer(today)(nextMonth) shouldBe More
    comparer(today)(lastMonth) shouldBe Less
    comparer(today)(nextYear) shouldBe More
    comparer(today)(lastYear) shouldBe Less

  behavior of "derived comparers with more fields"

  it should "derive a comparer with 4 fields" in:
    case class Case4(x1: Int, x2: Int, x3: Int, x4: Int) derives Comparer
    val comparer = summon[Comparer[Case4]]
    comparer(Case4(1, 2, 3, 4))(Case4(1, 2, 3, 0)) shouldBe Less
    comparer(Case4(1, 2, 3, 4))(Case4(2, 1, 3, 4)) shouldBe More
    comparer(Case4(1, 2, 3, 4))(Case4(1, 2, 3, 5)) shouldBe More

  it should "derive a comparer with 5 fields" in:
    case class Case5(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Long) derives Comparer
    val comparer = summon[Comparer[Case5]]
    comparer(Case5(1, 2, "3", Some(4), 99L))(Case5(1, 2, "3", Some(4), 98L)) shouldBe Less
    comparer(Case5(1, 2, "3", None, 99L))(Case5(1, 2, "3", None, 100L)) shouldBe More

  it should "derive a comparer with 5 fields for a really complex case class" in:
    case class Case5(x1: Seq[Int], x2: (Double, Double), x3: Try[Boolean], x4: Option[Int], x5: Unit) derives Comparer
    val x1 = Case5(Seq(1, 2), (1.0, 0.0), Success(true), Some(4), ())
    val x2 = Case5(Seq(1, 2), (1.0, 0.0), Success(true), Some(5), ())
    val x3 = Case5(Seq(1, 3), (1.0, 0.0), Success(true), Some(4), ())
    Compare(x1, x2) shouldBe Less
    Compare(x1, x3) shouldBe Less

  it should "derive a comparer with 5 Int fields" in:
    case class Case5(x1: Int, x2: Int, x3: Int, x4: Int, x5: Int) derives Comparer
    val comparer = summon[Comparer[Case5]]
    comparer(Case5(1, 2, 3, 4, 0))(Case5(2, 1, 3, 4, 0)) shouldBe More
    comparer(Case5(1, 2, 3, 4, 0))(Case5(1, 2, 3, 4, 1)) shouldBe More

  it should "derive a comparer with 6 fields" in:
    case class Case6(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long) derives Comparer
    val comparer = summon[Comparer[Case6]]
    comparer(Case6(1, 2, "3", Some(4), 0, 99L))(Case6(1, 2, "3", Some(4), 0, 98L)) shouldBe Less
    comparer(Case6(1, 2, "3", Some(4), 0, 99L))(Case6(2, 1, "3", Some(4), 0, 99L)) shouldBe More
    comparer(Case6(1, 2, "3", None, 0, 99L))(Case6(1, 2, "3", Some(4), 1, 100L)) shouldBe More

  it should "derive a comparer with 7 fields" in:
    case class Case7(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Boolean) derives Comparer
    val comparer = summon[Comparer[Case7]]
    comparer(Case7(1, 2, "3", Some(4), 0, 99L, x7 = true))(Case7(1, 2, "3", Some(5), 0, 99L, x7 = true)) shouldBe More
    comparer(Case7(1, 2, "3", None, 0, 99L, x7 = false))(Case7(1, 2, "3", Some(4), 1, 100L, x7 = false)) shouldBe More

  it should "derive a comparer with 8 fields" in:
    case class Case8(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Either[String, Int], x8: Boolean) derives Comparer
    val comparer = summon[Comparer[Case8]]
    comparer(Case8(1, 2, "3", Some(4), 0, 99L, Right(3), x8 = true))(Case8(1, 2, "3", Some(4), 0, 99L, Right(4), x8 = true)) shouldBe More
    comparer(Case8(1, 2, "3", None, 0, 99L, Left(""), x8 = false))(Case8(1, 2, "3", Some(4), 1, 100L, Left(""), x8 = false)) shouldBe More

  it should "derive a comparer with 9 fields" in:
    case class Case9(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Either[String, Int], x8: List[Int], x9: Boolean) derives Comparer
    val comparer = summon[Comparer[Case9]]
    comparer(Case9(1, 2, "3", Some(4), 0, 99L, Right(3), List(1), x9 = true))(Case9(1, 2, "3", Some(4), 0, 99L, Right(3), List(2, 1), x9 = true)) shouldBe More
    comparer(Case9(1, 2, "3", None, 0, 99L, Left(""), Nil, x9 = false))(Case9(1, 2, "3", Some(4), 1, 99L, Left(""), List(1), x9 = false)) shouldBe More

  it should "derive a comparer with 10 fields" in:
    case class Case10(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Either[String, Int], x8: Iterable[Int], x9: Iterable[Int], x10: Boolean) derives Comparer
    val comparer = summon[Comparer[Case10]]
    comparer(Case10(1, 2, "3", Some(4), 0, 1010L, Right(3), Seq(1), Seq(1), x10 = true))(Case10(1, 2, "3", Some(4), 0, 1010L, Right(3), Seq(2, 1), Seq(1), x10 = true)) shouldBe More
    comparer(Case10(1, 2, "3", None, 0, 1010L, Left(""), Nil, Array(1), x10 = false))(Case10(1, 2, "3", Some(4), 1, 1010L, Left(""), Seq(1), Array(1), x10 = false)) shouldBe More

  it should "derive a comparer with 11 fields" in:
    case class Case11(x1: Int, x2: Double, x3: String, x4: Option[Int], x5: Int, x6: Long, x7: Either[String, Int], x8: Iterable[Int], x9: Iterable[Int], x10: Boolean, x11: Try[Int]) derives Comparer
    val comparer = summon[Comparer[Case11]]
    comparer(Case11(1, 2, "3", Some(4), 0, 1010L, Right(3), Seq(2, 1), Seq(1), x10 = true, Success(1)))(Case11(1, 2, "3", Some(4), 0, 1010L, Right(3), Seq(2, 1), Seq(1), x10 = true, Success(2))) shouldBe More
    comparer(Case11(1, 2, "3", None, 0, 1010L, Left(""), Nil, Array(1), x10 = false, Success(0)))(Case11(1, 2, "3", None, 0, 1010L, Left(""), Nil, Array(1), x10 = false, Success(1))) shouldBe More

  it should "derive a comparer with 23 fields (beyond Scala 2's tuple limit of 22)" in:
    case class Case23(x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int,
                      x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int, x23: Int) derives Comparer
    val a = Case23(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
    val b = a.copy(x23 = 24)
    Compare(a, b) shouldBe Less
    Compare(b, a) shouldBe More
    Compare(a, a) shouldBe Same

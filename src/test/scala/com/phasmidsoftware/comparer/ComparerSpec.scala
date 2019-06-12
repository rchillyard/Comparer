/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import com.phasmidsoftware.comparer.Comparison.{Less, More}
import com.phasmidsoftware.sort.Sorted
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
  * @author scalaprof
  */
class ComparerSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  private val c1a = Composite(1, "a")
  private val c2a = Composite(2, "a")
  private val c1z = Composite(1, "z")

  case class DateJ(year: Int, month: Int, day: Int)

  object DateJ {

    trait OrderingDate extends Ordering[DateJ] {

      def compare(d1: DateJ, d2: DateJ): Int = {
        val cfy = d1.year.compareTo(d2.year)
        if (cfy != 0) cfy
        else {
          val cfm = d1.month.compareTo(d2.month)
          if (cfm != 0) cfm
          else d1.day.compareTo(d2.day)
        }
      }
    }

    implicit object OrderingDate extends OrderingDate

  }

  behavior of "Comparer"

  it should "compare Ints (1)" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer(1)(2) shouldBe Comparison.More
    comparer(1)(1) shouldBe Same
    comparer(2)(1) shouldBe Comparison.Less
  }

  it should "compare tupled" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer.tupled(1, 2) shouldBe Comparison.Less
    comparer.tupled(1, 1) shouldBe Same
    comparer.tupled(2, 1) shouldBe Comparison.More
  }

  it should "compare Booleans" in {
    val comparer: Comparer[Boolean] = Ordering[Boolean]
    comparer(true)(false) shouldBe Comparison.Less
    comparer(true)(true) shouldBe Same
    comparer(false)(true) shouldBe Comparison.More
  }

  it should "evaluate operators on Int" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer.>(1)(2) shouldBe true
    comparer.>(1)(1) shouldBe false
    comparer.>(2)(1) shouldBe false
    comparer.<(1)(2) shouldBe false
    comparer.<(1)(1) shouldBe false
    comparer.<(2)(1) shouldBe true
    comparer.<=(1)(2) shouldBe false
    comparer.<=(1)(1) shouldBe true
    comparer.<=(2)(1) shouldBe true
    comparer.>=(1)(2) shouldBe true
    comparer.>=(1)(1) shouldBe true
    comparer.>=(2)(1) shouldBe false
    comparer.==(1)(2) shouldBe false
    comparer.==(1)(1) shouldBe true
    comparer.==(2)(1) shouldBe false
    comparer.!=(1)(2) shouldBe true
    comparer.!=(1)(1) shouldBe false
    comparer.!=(2)(1) shouldBe true
  }

  it should "evaluate operators on Int (tupled)" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer.>(1, 2) shouldBe false
    comparer.>(1, 1) shouldBe false
    comparer.>(2, 1) shouldBe true
    comparer.<(1, 2) shouldBe true
    comparer.<(1, 1) shouldBe false
    comparer.<(2, 1) shouldBe false
    comparer.<=(1, 2) shouldBe true
    comparer.<=(1, 1) shouldBe true
    comparer.<=(2, 1) shouldBe false
    comparer.>=(1, 2) shouldBe false
    comparer.>=(1, 1) shouldBe true
    comparer.>=(2, 1) shouldBe true
    comparer.==(1, 2) shouldBe false
    comparer.==(1, 1) shouldBe true
    comparer.==(2, 1) shouldBe false
    comparer.!=(1, 2) shouldBe true
    comparer.!=(1, 1) shouldBe false
    comparer.!=(2, 1) shouldBe true
  }
  it should "map with function" in {
    val comparer: Comparer[Int] = Comparer.intComparer.map(_ flip)
    comparer(1)(2) shouldBe Comparison.Less
  }

  it should "invert" in {
    val comparer = Comparer.intComparer.invert
    comparer(1)(2) shouldBe Comparison.Less
  }

  it should "snap" in {
    val comparer1a: Comparer[Composite] = implicitly[Comparer[Int]].snap(_.i)
    val comparer1b: Comparer[Composite] = implicitly[Comparer[String]].snap(_.s)
    val comparer: Comparer[Composite] = comparer1b orElse comparer1a
    comparer(c1z)(c1a) shouldBe Less
    comparer(c2a)(c1a) shouldBe Less
    comparer(c2a)(c1z) shouldBe More
    comparer(c1a)(c1a) shouldBe Same
    comparer(c1a)(c2a) shouldBe More
    comparer(c1a)(c1z) shouldBe More
    val comparerAlt = comparer1a orElse comparer1b
    comparerAlt(c1z)(c1a) shouldBe Less
    comparerAlt(c2a)(c1a) shouldBe Less
    comparerAlt(c2a)(c1z) shouldBe Less
    comparerAlt(c1a)(c1a) shouldBe Same
    comparerAlt(c1a)(c2a) shouldBe More
    comparerAlt(c1a)(c1z) shouldBe More
  }

  it should "compose" in {
    val comparer1: Comparer[Int] = implicitly[Comparer[Int]]
    val comparer2: Comparer[String] = implicitly[Comparer[String]]
    val comparer3: Comparer[(Int, String)] = comparer1 compose comparer2
    val x: (Int, String) = Composite.unapply(c1a).get
    val y: (Int, String) = Composite.unapply(c1z).get
    comparer3.tupled(x, y) shouldBe Less
  }

  it should "compose using orElse" in {
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer3 = comparer1 orElse comparer2
    comparer3(c1z)(c1a) shouldBe Less
    comparer3(c2a)(c1a) shouldBe Less
    comparer3(c2a)(c1z) shouldBe More
    comparer3(c1a)(c1a) shouldBe Same
    comparer3(c1a)(c2a) shouldBe More
    comparer3(c1a)(c1z) shouldBe More
    val comparer4 = comparer2 orElse comparer1
    comparer4(c1z)(c1a) shouldBe Less
    comparer4(c2a)(c1a) shouldBe Less
    comparer4(c2a)(c1z) shouldBe Less
    comparer4(c1a)(c1a) shouldBe Same
    comparer4(c1a)(c2a) shouldBe More
    comparer4(c1a)(c1z) shouldBe More
  }

  it should "compose using orElseNot" in {
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer3 = comparer1 orElseNot comparer2
    comparer3(c1z)(c1a) shouldBe Less
    comparer3(c2a)(c1a) shouldBe More
    comparer3(c2a)(c1z) shouldBe More
    comparer3(c1a)(c1a) shouldBe Same
    comparer3(c1a)(c2a) shouldBe Less
    comparer3(c1a)(c1z) shouldBe More
    val comparer4 = comparer2 orElseNot comparer1
    comparer4(c1z)(c1a) shouldBe More
    comparer4(c2a)(c1a) shouldBe Less
    comparer4(c2a)(c1z) shouldBe Less
    comparer4(c1a)(c1a) shouldBe Same
    comparer4(c1a)(c2a) shouldBe More
    comparer4(c1a)(c1z) shouldBe Less
  }

  behavior of "partially applied comparer"

  it should "compare Ints (1)" in {
    val comparer: Comparer[Int] = Ordering[Int]
    val compareWithOne: Int => Comparison = comparer(1)
    compareWithOne(2) shouldBe Comparison.More
    compareWithOne(1) shouldBe Same
  }

  it should "evaluate operators on Int" in {
    val comparer: Comparer[Int] = Ordering[Int]
    val greaterThanOne: Int => Boolean = comparer.>(1)
    val greaterThanOrEqualToOne: Int => Boolean = comparer.>=(1)
    val equalToOne: Int => Boolean = comparer.==(1)
    val lessThanOne: Int => Boolean = comparer.<(1)
    val lessThanOrEqualToOne: Int => Boolean = comparer.<=(1)
    val notEqualToOne: Int => Boolean = comparer.!=(1)
    greaterThanOne(2) shouldBe true
    greaterThanOne(1) shouldBe false
    lessThanOne(2) shouldBe false
    lessThanOne(1) shouldBe false
    lessThanOrEqualToOne(2) shouldBe false
    lessThanOrEqualToOne(1) shouldBe true
    greaterThanOrEqualToOne(2) shouldBe true
    greaterThanOrEqualToOne(1) shouldBe true
    equalToOne(2) shouldBe false
    equalToOne(1) shouldBe true
    notEqualToOne(2) shouldBe true
    notEqualToOne(1) shouldBe false
  }

  it should "map with function" in {
    val comparer: Comparer[Int] = Comparer.intComparer.map(_ flip)
    val inverseCompareWithOne: Int => Comparison = comparer(1)
    inverseCompareWithOne(2) shouldBe Comparison.Less
  }

  it should "invert" in {
    val comparer = Comparer.intComparer.invert
    val inverseCompareWithOne: Int => Comparison = comparer(1)
    inverseCompareWithOne(2) shouldBe Comparison.Less
  }

  it should "snap" in {
    val comparer1a: Comparer[Composite] = implicitly[Comparer[Int]].snap(_.i)
    val comparer1b: Comparer[Composite] = implicitly[Comparer[String]].snap(_.s)
    val comparer: Comparer[Composite] = comparer1b orElse comparer1a
    comparer(c1z)(c1a) shouldBe Less
    comparer(c2a)(c1a) shouldBe Less
    comparer(c2a)(c1z) shouldBe More
    comparer(c1a)(c1a) shouldBe Same
    comparer(c1a)(c2a) shouldBe More
    comparer(c1a)(c1z) shouldBe More
    val comparerAlt = comparer1a orElse comparer1b
    comparerAlt(c1z)(c1a) shouldBe Less
    comparerAlt(c2a)(c1a) shouldBe Less
    comparerAlt(c2a)(c1z) shouldBe Less
    comparerAlt(c1a)(c1a) shouldBe Same
    comparerAlt(c1a)(c2a) shouldBe More
    comparerAlt(c1a)(c1z) shouldBe More
  }

  it should "snap on Date object" in {
    val ic = implicitly[Comparer[Int]]
    val comparerY: Comparer[DateJ] = ic.snap(_.year)
    val comparerM: Comparer[DateJ] = ic.snap(_.month)
    val comparerD: Comparer[DateJ] = ic.snap(_.day)
    val comparer: Comparer[DateJ] = comparerY orElse comparerM orElse comparerD
    val today = DateJ(2019, 6, 5)
    val tomorrow = DateJ(2019, 6, 6)
    val yesterday = DateJ(2019, 6, 4)
    val nextMonth = DateJ(2019, 7, 5)
    val lastMonth = DateJ(2019, 5, 5)
    val nextYear = DateJ(2020, 6, 5)
    val lastYear = DateJ(2018, 6, 5)
    comparer.tupled(today, today) shouldBe Same
    comparer.tupled(tomorrow, today) shouldBe More
    comparer.tupled(today, tomorrow) shouldBe Less
    comparer.tupled(today, yesterday) shouldBe More
    comparer.tupled(today, nextMonth) shouldBe Less
    comparer.tupled(today, lastMonth) shouldBe More
    comparer.tupled(today, nextYear) shouldBe Less
    comparer.tupled(today, lastYear) shouldBe More
  }

  it should "compose" in {
    val comparer1: Comparer[Int] = implicitly[Comparer[Int]]
    val comparer2: Comparer[String] = implicitly[Comparer[String]]
    val comparer3: Comparer[(Int, String)] = comparer1 compose comparer2
    val x: (Int, String) = Composite.unapply(c1a).get
    val y: (Int, String) = Composite.unapply(c1z).get
    comparer3(x)(y) shouldBe More
  }

  it should "compose using orElse" in {
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer3 = comparer1 orElse comparer2
    comparer3(c1z)(c1a) shouldBe Less
    comparer3(c2a)(c1a) shouldBe Less
    comparer3(c2a)(c1z) shouldBe More
    comparer3(c1a)(c1a) shouldBe Same
    comparer3(c1a)(c2a) shouldBe More
    comparer3(c1a)(c1z) shouldBe More
    val comparer4 = comparer2 orElse comparer1
    comparer4(c1z)(c1a) shouldBe Less
    comparer4(c2a)(c1a) shouldBe Less
    comparer4(c2a)(c1z) shouldBe Less
    comparer4(c1a)(c1a) shouldBe Same
    comparer4(c1a)(c2a) shouldBe More
    comparer4(c1a)(c1z) shouldBe More
  }

  it should "implement different" in {
    val target1 = Comparer.different[Int](less = true)
    target1(1)(1) shouldBe Less
    val target2 = Comparer.different[Int](less = false)
    target2(1)(1) shouldBe More
  }

  it should "implement same" in {
    val target1 = Comparer.same[Int]
    target1(1)(2) shouldBe Same
  }

  behavior of "merge"
  it should "work" in {
    val l1 = List(1, 5, 8, 10, 11, 15)
    val l2 = List(3, 4, 9, 12, 14, 16)
    Sorted.merge(l1, l2) shouldBe List(1, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16)
  }

  behavior of "date comparison"
  it should "java-style compare" in {
    val today = DateJ(2019, 6, 5)
    val tomorrow = DateJ(2019, 6, 6)
    val yesterday = DateJ(2019, 6, 4)
    val nextMonth = DateJ(2019, 7, 5)
    val lastMonth = DateJ(2019, 5, 5)
    val nextYear = DateJ(2020, 6, 5)
    val lastYear = DateJ(2018, 6, 5)
    val ordering = implicitly[Ordering[DateJ]]
    ordering.compare(today, today) shouldBe 0
    ordering.compare(tomorrow, today) shouldBe 1
    ordering.compare(today, tomorrow) shouldBe -1
    ordering.compare(today, yesterday) shouldBe 1
    ordering.compare(today, nextMonth) shouldBe -1
    ordering.compare(today, lastMonth) shouldBe 1
    ordering.compare(today, nextYear) shouldBe -1
    ordering.compare(today, lastYear) shouldBe 1
  }

  it should "compare in a somewhat more functional way" in {
    val today = DateJ(2019, 6, 5)
    val tomorrow = DateJ(2019, 6, 6)
    val yesterday = DateJ(2019, 6, 4)
    val nextMonth = DateJ(2019, 7, 5)
    val lastMonth = DateJ(2019, 5, 5)
    val nextYear = DateJ(2020, 6, 5)
    val lastYear = DateJ(2018, 6, 5)

    val orderingYear: Comparer[DateJ] = t1 => t2 => Comparison(t1.year)(t2.year)
    val orderingMonth: Comparer[DateJ] = t1 => t2 => Comparison(t1.month)(t2.month)
    val orderingDay: Comparer[DateJ] = t1 => t2 => Comparison(t1.day)(t2.day)
    implicit val orderingDate: Comparer[DateJ] = orderingYear orElse orderingMonth orElse orderingDay
    Comparison.compare(today, today) shouldBe Same
    Comparison.compare(tomorrow, today) shouldBe More
    Comparison.compare(today, tomorrow) shouldBe Less
    Comparison.compare(today, yesterday) shouldBe More
    Comparison.compare(today, nextMonth) shouldBe Less
    Comparison.compare(today, lastMonth) shouldBe More
    Comparison.compare(today, nextYear) shouldBe Less
    Comparison.compare(today, lastYear) shouldBe More


  }

  it should "functional-style compare (curried)" in {
    val today = DateF(2019, 6, 5)
    val tomorrow = DateF(2019, 6, 6)
    val yesterday = DateF(2019, 6, 4)
    val nextMonth = DateF(2019, 7, 5)
    val lastMonth = DateF(2019, 5, 5)
    val nextYear = DateF(2020, 6, 5)
    val lastYear = DateF(2018, 6, 5)
    Comparison(today)(today) shouldBe Same
    Comparison(today)(tomorrow) shouldBe More
    Comparison(tomorrow)(today) shouldBe Less
    Comparison(today)(yesterday) shouldBe Less
    Comparison(today)(nextMonth) shouldBe More
    Comparison(today)(lastMonth) shouldBe Less
    Comparison(today)(nextYear) shouldBe More
    Comparison(today)(lastYear) shouldBe Less
  }

  it should "functional-style compare (partially-applied)" in {
    val today = DateF(2019, 6, 5)
    val tomorrow = DateF(2019, 6, 6)
    val yesterday = DateF(2019, 6, 4)
    val nextMonth = DateF(2019, 7, 5)
    val lastMonth = DateF(2019, 5, 5)
    val nextYear = DateF(2020, 6, 5)
    val lastYear = DateF(2018, 6, 5)
    val comparedToToday = Comparison(today) _
    comparedToToday(today) shouldBe Same
    comparedToToday(tomorrow) shouldBe More
    comparedToToday(yesterday) shouldBe Less
    comparedToToday(nextMonth) shouldBe More
    comparedToToday(lastMonth) shouldBe Less
    comparedToToday(nextYear) shouldBe More
    comparedToToday(lastYear) shouldBe Less
  }

  it should "functional-style compare (tupled)" in {
    val today = DateF(2019, 6, 5)
    val tomorrow = DateF(2019, 6, 6)
    val yesterday = DateF(2019, 6, 4)
    val nextMonth = DateF(2019, 7, 5)
    val lastMonth = DateF(2019, 5, 5)
    val nextYear = DateF(2020, 6, 5)
    val lastYear = DateF(2018, 6, 5)
    import Comparison.compare
    compare(today, today) shouldBe Same
    compare(today, tomorrow) shouldBe Less
    compare(tomorrow, today) shouldBe More
    compare(today, yesterday) shouldBe More
    compare(today, nextMonth) shouldBe Less
    compare(today, lastMonth) shouldBe More
    compare(today, nextYear) shouldBe Less
    compare(today, lastYear) shouldBe More
  }
}

case class Composite(i: Int, s: String)

object Composite {

  implicit val comparer: Comparer[Composite] = Comparer.same[Composite] :| (_.s) :| (_.i)

  object OrderingCompositeInt extends Ordering[Composite] {
    def compare(x: Composite, y: Composite): Int = x.i.compare(y.i)
  }

  object OrderingCompositeString extends Ordering[Composite] {
    def compare(x: Composite, y: Composite): Int = x.s.compare(y.s)
  }

}


case class DateF(year: Int, month: Int, day: Int)

object DateF {
  implicit val dateComparer: Comparer[DateF] = Comparer.same[DateF] :| (_.year) :| (_.month) :| (_.day)

  //  implicit val dateComparer: Comparer[DateF] = Comparer(_.year, _.month, _.day)

}

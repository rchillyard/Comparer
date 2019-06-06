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
class ComparerSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "Comparer"

  it should "compare Ints (1)" in {
    val comparer: Comparer[Int] = Ordering[Int]
    comparer(1, 2) shouldBe Comparison.Less
    comparer(1, 1) shouldBe Same
    comparer(2, 1) shouldBe Comparison.More
  }
  it should "evaluate operators on Int" in {
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
    comparer((1, 2)) shouldBe Comparison.More
  }
  it should "invert" in {
    val comparer = Comparer.intComparer.invert
    comparer((1, 2)) shouldBe Comparison.More
  }

  private val c1a = Composite(1, "a")
  private val c2a = Composite(2, "a")
  private val c1z = Composite(1, "z")

  it should "snap" in {
    val comparer1a: Comparer[Composite] = implicitly[Comparer[Int]].snap(_.i)
    val comparer1b: Comparer[Composite] = implicitly[Comparer[String]].snap(_.s)
    val comparer: Comparer[Composite] = comparer1b orElse comparer1a
    comparer(c1a, c1z) shouldBe Less
    comparer(c1a, c1z) shouldBe Less
    comparer(c1a, c2a) shouldBe Less
    comparer(c1z, c2a) shouldBe More
    comparer(c1a, c1a) shouldBe Same
    comparer(c2a, c1a) shouldBe More
    comparer(c1z, c1a) shouldBe More
    val comparerAlt = comparer1a orElse comparer1b
    comparerAlt(c1a, c1z) shouldBe Less
    comparerAlt(c1a, c2a) shouldBe Less
    comparerAlt(c1z, c2a) shouldBe Less
    comparerAlt(c1a, c1a) shouldBe Same
    comparerAlt(c2a, c1a) shouldBe More
    comparerAlt(c1z, c1a) shouldBe More
  }

  it should "compose" in {
    val comparer1: Comparer[Int] = implicitly[Comparer[Int]]
    val comparer2: Comparer[String] = implicitly[Comparer[String]]
    val comparer3: Comparer[(Int, String)] = comparer1 compose comparer2
    val x: (Int, String) = Composite.unapply(c1a).get
    val y: (Int, String) = Composite.unapply(c1z).get
    comparer3(x -> y) shouldBe Less
    //    comparer3(Composite(1, "a") -> Composite(1, "z")) shouldBe less


  }
  it should "compose using orElse" in {
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer3 = comparer1 orElse comparer2
    comparer3(c1a, c1z) shouldBe Less
    comparer3(c1a, c2a) shouldBe Less
    comparer3(c1z, c2a) shouldBe More
    comparer3(c1a, c1a) shouldBe Same
    comparer3(c2a, c1a) shouldBe More
    comparer3(c1z, c1a) shouldBe More
    val comparer4 = comparer2 orElse comparer1
    comparer4(c1a, c1z) shouldBe Less
    comparer4(c1a, c2a) shouldBe Less
    comparer4(c1z, c2a) shouldBe Less
    comparer4(c1a, c1a) shouldBe Same
    comparer4(c2a, c1a) shouldBe More
    comparer4(c1z, c1a) shouldBe More
  }

  behavior of "Sorted"

  it should "sort List[Int]" in {
    val list = List(3, 1, 2)
    val sorted = Sorted(list)
    sorted() shouldBe List(1, 2, 3)
  }
  it should "sort List[String]" in {
    val list = List("b", "c", "a")
    val sorted = Sorted(list)
    sorted() shouldBe List("a", "b", "c")
  }
  it should "sort List[Double] using create" in {
    val list = List(3.0, 1.5, 2.4)
    val sorted = Sorted.create(list)
    sorted() shouldBe List(1.5, 2.4, 3.0)
  }
  it should "sort List[Char] given an explicit Comparer" in {
    val charComparer: Comparer[Char] = Ordering[Char]
    val list = List('b', 'c', 'a')
    val sorted = Sorted(list)(charComparer.invert)
    sorted() shouldBe List('c', 'b', 'a')
  }
  private val c2b = Composite(2, "b")
  private val c3c = Composite(3, "c")
  it should "sort List[Composite] by Int then String the easy way" in {
    val list = List(c3c, c1a, c1z, c2b)
    val sorted = Sorted(list)(Comparer.same[Composite] :| (_.i) :| (_.s))
    sorted() shouldBe List(c1a, c1z, c2b, c3c)
  }
  it should "sort List[Composite] by Int then String" in {
    val list = List(c3c, c1a, c1z, c2b)
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeString
    val sorted = Sorted(list)(comparer1).sort(comparer2)
    sorted() shouldBe List(c1a, c1z, c2b, c3c)
  }
  it should "sort List[Composite] by String then Int the really easy way" in {
    val list = List(c3c, c1a, c1z, c2b)
    val sorted = Sorted(list)
    sorted() shouldBe List(c1a, c2b, c3c, c1z)
  }
  it should "sort List[Composite] by String then Int" in {
    val list = List(c3c, c1a, c1z, c2b)
    val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val sorted = Sorted(list)(comparer1).sort(comparer2)
    sorted() shouldBe List(c1a, c2b, c3c, c1z)
  }

  it should "sort asynchronously" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val list = List(3, 1, 2)
    val sorted = Sorted.create(list)
    val xsf = sorted.async
    whenReady(xsf) { xs => xs shouldBe List(1, 2, 3) }
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
    today.compare(today) shouldBe 0
    tomorrow.compare(today) shouldBe 1
    today.compare(tomorrow) shouldBe -1
    today.compare(yesterday) shouldBe 1
    today.compare(nextMonth) shouldBe -1
    today.compare(lastMonth) shouldBe 1
    today.compare(nextYear) shouldBe -1
    today.compare(lastYear) shouldBe 1
  }

  it should "functional-style compare" in {
    val today = DateF(2019, 6, 5)
    val tomorrow = DateF(2019, 6, 6)
    val yesterday = DateF(2019, 6, 4)
    val nextMonth = DateF(2019, 7, 5)
    val lastMonth = DateF(2019, 5, 5)
    val nextYear = DateF(2020, 6, 5)
    val lastYear = DateF(2018, 6, 5)

    Comparison(today, today) shouldBe Same
    Comparison(today, tomorrow) shouldBe Less
    Comparison(tomorrow, today) shouldBe More
    Comparison(today, yesterday) shouldBe More
    Comparison(today, nextMonth) shouldBe Less
    Comparison(today, lastMonth) shouldBe More
    Comparison(today, nextYear) shouldBe Less
    Comparison(today, lastYear) shouldBe More
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

case class DateJ(year: Int, month: Int, day: Int) extends Ordered[DateJ] {
  def compare(that: DateJ): Int = {
    val cfy = year.compareTo(that.year)
    if (cfy!=0) cfy
    else {
      val cfm = month.compareTo(that.month)
      if (cfm!=0) cfm
      else day.compareTo(that.day)
    }
  }
}

object DateJ {
  implicit object dateOrdering extends Ordering[DateF] {
    def compare(d1: DateF, d2: DateF): Int = {
      val cfy = d1.year.compareTo(d2.year)
      if (cfy!=0) cfy
      else {
        val cfm = d1.month.compareTo(d2.month)
        if (cfm!=0) cfm
        else d1.day.compareTo(d2.day)
      }
    }
  }
}

case class DateF(year: Int, month: Int, day: Int)

object DateF {
  implicit val dateComparer: Comparer[DateF] = Comparer.same[DateF] :| (_.year) :| (_.month) :| (_.day)

  //  implicit val dateComparer: Comparer[DateF] = Comparer(_.year, _.month, _.day)

}

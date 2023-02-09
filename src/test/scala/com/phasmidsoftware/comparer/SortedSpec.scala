/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import com.phasmidsoftware.sort.Sorted
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{flatspec, matchers}

import scala.language.postfixOps
import scala.util.Random

/**
  * @author scalaprof
  */
class SortedSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers with Futures with ScalaFutures {

  behavior of "Sorted"

  private val c1a = Composite(1, "a")
  private val c2a = Composite(2, "a")
  private val c1z = Composite(1, "z")
  private val c2b = Composite(2, "b")
  private val c3c = Composite(3, "c")

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

  it should "sort List[Double]" in {
    val list = List(3.0, 1.5, 2.4)
    val sorted = Sorted(list)
    sorted() shouldBe List(1.5, 2.4, 3.0)
  }

  it should "sort List[Double] using create" in {
    val list = List(3.0, 1.5, 2.4)
    import Ordering.Double.TotalOrdering
    val sorted = Sorted.create(list)
    sorted() shouldBe List(1.5, 2.4, 3.0)
  }

  it should "sort List[Float] using create because there isn't an implicitly defined Comparer for float" in {
    val list = List(3.0F, 1.5F, 2.4F)
    import Ordering.Float.TotalOrdering
    val sorted = Sorted.create(list)
    sorted() shouldBe List(1.5F, 2.4F, 3.0F)
  }

  it should "sort List[Char] given an explicit Comparer" in {
    val charComparer: Comparer[Char] = Ordering[Char]
    val list = List('b', 'c', 'a')
    implicit val comparer: Comparer[Char] = charComparer.invert
    val sorted = Sorted(list)
    sorted() shouldBe List('c', 'b', 'a')
  }

  it should "sort List[Composite] by Int then String the easy way" in {
    val list = List(c3c, c1a, c1z, c2b)
    implicit val comparer: Comparer[Composite] = Comparer.same[Composite] :| (_.i) :| (_.s)
    val sorted = Sorted(list)
    sorted() shouldBe List(c1a, c1z, c2b, c3c)
  }
  it should "sort List[Composite] by String then the inverse of Int" in {
    val list = List(c3c, c1a, c1z, c2b, c2a)
    implicit val comparer: Comparer[Composite] = Comparer.same[Composite] :| (_.s) :|! (_.i)
    val sorted = Sorted(list)
    sorted() shouldBe List(c2a, c1a, c2b, c3c, c1z)
  }

  it should "sort List[DateF] by explicit create" in {
    val list = List(c3c, c1a, c1z, c2b)
    val comparerS = implicitly[Comparer[String]].snap[Composite](_.s)
    val comparerI = implicitly[Comparer[Int]].snap[Composite](_.i)
    implicit val comparer: Comparer[Composite] = Comparer.create(comparerI, comparerS)
    val sorted = Sorted(list)
    sorted() shouldBe List(c1a, c1z, c2b, c3c)
  }
  it should "sort List[DateF] by explicit apply" in {
    val d1 = DateF(1951, 11, 12)
    val d2 = DateF(1963, 1, 5)
    val d3 = DateF(1978, 9, 12)
    val d4 = DateF(1984, 6, 6)
    val d5 = DateF(2000, 3, 2)
    val list = List(d3, d5, d1, d2, d4)
    implicit val comparer: Comparer[DateF] = Comparer[DateF, Int](_.year, _.month, _.day)
    val sorted: Sorted[DateF] = Sorted(list)
    sorted() shouldBe List(d1, d2, d3, d4, d5)
  }
  it should "sort List[Composite] by Int then String" in {
    val list = List(c3c, c1a, c1z, c2b)
    implicit val comparer1: Comparer[Composite] = Composite.OrderingCompositeInt
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeString
    val sorted = Sorted(list).sort(comparer2)
    sorted() shouldBe List(c1a, c1z, c2b, c3c)
  }
  it should "sort List[Composite] by String then Int the really easy way" in {
    val list = List(c3c, c1a, c1z, c2b)
    val sorted = Sorted(list)
    sorted() shouldBe List(c1a, c2b, c3c, c1z)
  }
  it should "sort List[Composite] by String then Int" in {
    val list = List(c3c, c1a, c1z, c2b)
    implicit val comparer1: Comparer[Composite] = Composite.OrderingCompositeString
    val comparer2: Comparer[Composite] = Composite.OrderingCompositeInt
    val sorted = Sorted(list).sort(comparer2)
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
  it should "sort in parallel" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val list = List(3, 1, 2, 4, 6, 5)
    val sorted = Sorted.create(list)
    val xsf = sorted.parallel
    whenReady(xsf) { xs => xs shouldBe List(1, 2, 3, 4, 5, 6) }
  }

  it should "merge sort not in parallel" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val list = List(3, 1, 2, 4, 6, 5, 13, 7, 8)
    import Sorted._
    val xsf = mergeSort(list)
    whenReady(xsf) { xs => xs shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 13) }
  }

  it should "merge sort large in parallel" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val r = Random
    val list = LazyList.from(1).take(1000).map(_ => r.nextInt())
    import Sorted._
    val xsf = mergeSort(list)
    whenReady(xsf) { xs => verify(xs) }
  }

}


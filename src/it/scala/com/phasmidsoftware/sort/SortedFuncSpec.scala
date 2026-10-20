/*
 * Copyright (c) 2024. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.sort

import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{flatspec, matchers}

import scala.language.postfixOps
import scala.util.Random

/**
  * @author scalaprof
  */
class SortedFuncSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers with Futures with ScalaFutures {

  behavior of "merge"

  // TODO Find out why this takes so long
  it should "sort in parallel" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val r = Random
    val list = LazyList.from(1).take(10000).map(_ => r.nextInt())
    val sorted = Sorted.create(list)
    val xsf = sorted.parallel
    whenReady(xsf, Timeout(Span(2, Seconds))) {
      case xs if !Sorted.verify(xs) => fail("not sorted")
      case _ =>
    }
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


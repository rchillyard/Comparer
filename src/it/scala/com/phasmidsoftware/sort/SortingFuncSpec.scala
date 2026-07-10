/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.sort

import com.phasmidsoftware.util.RandomState
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{flatspec, matchers}


/**
  * @author scalaprof
  */
class SortingFuncSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers with Futures with ScalaFutures {

  behavior of "Quick Sort"

  it should "sort List[Long]" in {
    val list = RandomState(0L).stream.take(1000).toList
    val ordered = list.sorted
    ordered.take(5) shouldBe Array(-9216660707259175019L, -9213036029692010464L, -9206926828484168278L, -9181452172432867415L, -9162073249424238457L)
    ordered.reverse.take(5) shouldBe Array(9195196935716632702L, 9181861996276601465L, 9179403599629101794L, 9174553474901295787L, 9151076946114612916L)
  }

  behavior of "Merge Sort"

  it should "sort List[Long]" in {
    val array = RandomState(0L).stream.take(1000).toArray
    Sorting.mergeSort(array)
    Sorted.verify(array.toList) shouldBe true
    array.reverse.take(5) shouldBe Array(9195196935716632702L, 9181861996276601465L, 9179403599629101794L, 9174553474901295787L, 9151076946114612916L)
  }
}

/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.sort

import com.phasmidsoftware.comparer.DateJ
import com.phasmidsoftware.generic.Unpack
import com.phasmidsoftware.util.RandomState
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.scalatest.{FlatSpec, Matchers}


/**
  * @author scalaprof
  */
class SortingSpec extends FlatSpec with Matchers with Futures with ScalaFutures {

  behavior of "Insertion Sort"

  it should "sort List[Int]" in {
    val list = Array(3, 1, 2)
    Sorting.insertionSort(list)
    list shouldBe Array(1, 2, 3)
  }
  it should "sort List[String]" in {
    val list = Array("b", "c", "a")
    Sorting.insertionSort(list)
    list shouldBe Array("a", "b", "c")
  }
  it should "sort List[Double] using create" in {
    val list = Array(3.0, 1.5, 2.4)
    Sorting.insertionSort(list)
    list shouldBe Array(1.5, 2.4, 3.0)
  }

  behavior of "Quick Sort"

  it should "sort List[Long]" in {
    val list = RandomState(0L).stream.take(100).toArray
    Sorting.sort(list)
    list.reverse.take(5) shouldBe Array(9054633673849498218L, 8937230293740383692L, 8613213585075034408L, 8543763135442756639L, 8358116205139703580L)
  }

  it should "sort List[DateJ]" in {
    def getRandomIntStream(n: Int): Stream[Int] = {
      def modulo(n: Int): Long => Int = { l: Long => math.abs(l.toInt) % n }

      RandomState(0L).stream.take(100).map(modulo(n))
    }

    val tuples = getRandomIntStream(2030) zip getRandomIntStream(12) zip getRandomIntStream(31)
    val toDateJ: ((Int, Int, Int)) => DateJ = (DateJ.apply _).tupled
    val list = ((tuples.map(Unpack.unpack) map toDateJ) take 5).toArray
    Sorting.sort(list)
    list.take(5) shouldBe Array(DateJ(0, 0, 0), DateJ(59, 3, 20), DateJ(560, 8, 0), DateJ(1675, 11, 25), DateJ(1976, 10, 20))
  }

  behavior of "Merge Sort"

  it should "sort List[Long]" in {
    val list = RandomState(0L).stream.take(100).toArray
    Sorting.mergeSort(list)
    list.reverse.take(5) shouldBe Array(9054633673849498218L, 8937230293740383692L, 8613213585075034408L, 8543763135442756639L, 8358116205139703580L)
  }
}

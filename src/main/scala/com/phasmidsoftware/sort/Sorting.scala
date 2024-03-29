/*
 * This code is borrowed from math.spire with love, if not actual permission.
 *
 * It is not really part of the Comparer project and is only here to help with practical usage.
 * It could easily be removed.
 *
 * NOTE: Therefore, we do not include this module in Codacy reviews.
 */

package com.phasmidsoftware.sort

import scala.reflect.ClassTag

/**
  * Interface for a sorting strategy object.
  */
trait Sort extends Any {
  def sort[A: Ordering : ClassTag](data: Array[A]): Unit
}

/**
  * Simple implementation of insertion sort.
  *
  * Works for small arrays but due to quadratic complexity is not generally good, unless array is partially sorted.
  */
object InsertionSort extends Sort {
  final def sort[A: Ordering : ClassTag](data: Array[A]): Unit =
    sort(data, 0, data.length)

  final def sort[A: Ordering](data: Array[A], start: Int, end: Int): Unit = {
    val o = implicitly[Ordering[A]]
    var i = start + 1
    while (i < end) {
      val item = data(i)
      var hole = i
      while (hole > start && o.gt(data(hole - 1), item)) {
        data(hole) = data(hole - 1)
        hole -= 1
      }
      data(hole) = item
      i += 1
    }
  }
}

/**
  * In-place merge sort implementation. This sort is stable but does mutate
  * the given array. It is an in-place sort but it does allocate a temporary
  * array of the same size as the input. It uses InsertionSort for sorting very
  * small arrays.
  */
object MergeSort extends Sort {
  @inline final def startWidth: Int = 8

  @inline final def startStep: Int = 16

  final def sort[A: Ordering : ClassTag](data: Array[A]): Unit = {
    val len = data.length

    if (len <= startStep) return InsertionSort.sort(data)

    var buf1: Array[A] = data
    var buf2: Array[A] = new Array[A](len)
    var tmp: Array[A] = null

    var i = 0
    var limit = len - startWidth
    while (i < limit) {
      InsertionSort.sort(data, i, i + startWidth)
      i += startWidth
    }
    if (i < len) InsertionSort.sort(data, i, len)
    var width = startWidth
    var step = startStep
    while (width < len) {
      i = 0
      limit = len - step
      while (i < limit) {
        merge(buf1, buf2, i, i + width, i + step)
        i += step
      }
      while (i < len) {
        merge(buf1, buf2, i, Math.min(i + width, len), len)
        i += step
      }
      tmp = buf2
      buf2 = buf1
      buf1 = tmp

      width *= 2
      step *= 2
    }

    if (!(buf1 sameElements data)) System.arraycopy(buf1, 0, data, 0, len)
  }

  /**
    * Helper method for mergeSort, used to do a single "merge" between two
    * sections of the input array. The start, mid and end parameters denote the
    * left and right ranges of the input to merge, as well as the area of the
    * output to write to.
    */
  @inline final def merge[A: Ordering : ClassTag](in: Array[A], out: Array[A], start: Int, mid: Int, end: Int): Unit = {
    val o = implicitly[Ordering[A]]
    var ii = start
    var jj = mid
    var kk = start
    while (kk < end) {
      if (ii < mid && (jj >= end || o.lteq(in(ii), in(jj)))) {
        out(kk) = in(ii)
        ii += 1
      } else {
        out(kk) = in(jj)
        jj += 1
      }
      kk += 1
    }
  }
}

/**
  * In-place quicksort implementation. It is not stable, but does not allocate
  * extra space (other than stack). Like MergeSort, it uses InsertionSort for
  * sorting very small arrays.
  */
object QuickSort {
  @inline final def limit: Int = 16

  final def sort[A: Ordering : ClassTag](data: Array[A]): Unit = qsort(data, 0, data.length - 1)

  final def qsort[A: Ordering : ClassTag](data: Array[A], left: Int, right: Int): Unit = {

    if (right - left < limit) return InsertionSort.sort(data, left, right + 1)

    val pivot = left + (right - left) / 2
    val next = partition(data, left, right, pivot)
    qsort(data, left, next - 1)
    qsort(data, next + 1, right)
  }

  final def partition[A: Ordering : ClassTag](data: Array[A], left: Int, right: Int, pivot: Int): Int = {
    val o = implicitly[Ordering[A]]

    val value = data(pivot)

    //swap(pivot, right)
    var tmp = data(pivot)
    data(pivot) = data(right)
    data(right) = tmp

    var store = left
    var i = left
    while (i < right) {
      if (o.lt(data(i), value)) {
        //swap(i, store)
        tmp = data(i)
        data(i) = data(store)
        data(store) = tmp
        store += 1
      }
      i += 1
    }
    //swap(store, right)
    tmp = data(store)
    data(store) = data(right)
    data(right) = tmp
    store
  }
}

/**
  * Object providing in-place sorting capability for arrays.
  *
  * Sorting.sort() uses quickSort() by default (in-place, not stable, generally
  * fastest but might hit bad cases where it's quadratic. Also provides
  * mergeSort() (in-place, stable, uses extra memory, still pretty fast) and
  * insertionSort(), which is slow except for small arrays.
  */
object Sorting {
  final def sort[A: Ordering : ClassTag](data: Array[A]): Unit = quickSort(data)

  final def insertionSort[A: Ordering : ClassTag](data: Array[A]): Unit = InsertionSort.sort(data)

  final def mergeSort[A: Ordering : ClassTag](data: Array[A]): Unit = MergeSort.sort(data)

  final def quickSort[A: Ordering : ClassTag](data: Array[A]): Unit = QuickSort.sort(data)
}

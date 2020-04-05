/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}

/**
  * Case class Sorted to enable different types of sort on a sequence.
  *
  * @param ts a sequence of T values/
  * @tparam T the underlying type of the sequence (implicitly provides Comparer[T])
  */
case class Sorted[T: Comparer](ts: Seq[T]) extends (() => Seq[T]) {

  private val ct = implicitly[Comparer[T]]

  implicit val ordering: Ordering[T] = ct.toOrdering

  def sort(o: Comparer[T]): Sorted[T] = Sorted(ts)(ct orElse o)

  /**
    * Use the system sort, based on the ordering specified by the implicit Comparer.
    *
    * @return a sorted sequence of T.
    */
  def apply: Seq[T] = ts.sorted

  /**
    * Use the system sort (see apply) but the sort will be performed asynchronously.
    *
    * @param ec an ExecutionContext.
    * @return a sorted sequence of T, wrapped in Future.
    */
  def async(implicit ec: ExecutionContext): Future[Seq[T]] = Future(apply)

  /**
    * Use the parallel merge sort.
    *
    * @param ec an ExecutionContext.
    * @return a sorted sequence of T, wrapped in Future.
    */
  def parallel(implicit ec: ExecutionContext): Future[Seq[T]] = Sorted.mergeSort(ts)
}

object Sorted {
  /**
    * Method to create a Sorted(ts).
    * The result can then be sorted using sort, async, parSort, apply (sorted).
    *
    * @param ts a sequence of T values.
    * @tparam T the type of the elements of ts (provides implicit Ordering[T]).
    * @return an instance of Sorted.
    */
  def create[T: Ordering](ts: Seq[T]): Sorted[T] = Sorted(ts)(implicitly[Ordering[T]])

  /**
    * Verify that the sequence ts is in face in ascending order.
    *
    * @param ts the sequence of T values.
    * @tparam T the type of the elements of ts.
    * @return true if all consecutive pairs of elements are in the correct order.
    */
  def verify[T: Comparer](ts: Seq[T]): Boolean = ts.zip(ts.tail).forall(z => implicitly[Comparer[T]].<=(z._2)(z._1))

  /**
    * Perform sort in exactly two parallel processes, each using the system sort, based on the
    * implicit Ordering[T] parameter.
    *
    * @param tst a tuple of two Seq[T].
    * @param ec  an ExecutionContext
    * @tparam T the underlying type to be sorted on, providing an implicit Ordering[T].
    * @return a sorted sequence of T, wrapped in Future.
    */
  def parSort[T: Ordering](tst: (Seq[T], Seq[T]))(implicit ec: ExecutionContext): Future[Seq[T]] = map2(Future(tst._1.sorted), Future(tst._2.sorted))(merge)

  /**
    * Invoke parSort on two equal partitions of ts.
    * TODO: implement parallel merge sort properly.
    *
    * @param ts the sequence of T values to be sorted.
    * @param ec an ExecutionContext
    * @tparam T the underlying type to be sorted on, providing an implicit Ordering[T].
    * @return a sorted sequence of T, wrapped in Future.
    */
  def mergeSort[T: Ordering](ts: Seq[T])(implicit ec: ExecutionContext): Future[Seq[T]] = parSort(ts splitAt (ts.length / 2))

  /**
    * Method to merge the sorted sequences ts1 and ts2.
    * NOTE: could be private but we cannot test it directly then (because it has a second parameter set).
    *
    * @param ts1 a sorted sequence of T values to be merged.
    * @param ts2 a sorted sequence of T values to be merged.
    * @tparam T the underlying type to be sorted on, providing an implicit Ordering[T].
    * @return a sorted sequence of T.
    */
  def merge[T: Ordering](ts1: Seq[T], ts2: Seq[T]): Seq[T] = {
    val ordering = implicitly[Ordering[T]]

    @tailrec def inner(r: Seq[T], xs: Seq[T], ys: Seq[T]): Seq[T] = (xs, ys) match {
      case (_, Nil) => r ++ xs
      case (Nil, _) => r ++ ys
      case (x :: xs1, y :: _) if ordering.lt(x, y) => inner(r :+ x, xs1, ys)
      case (_, y :: ys1) => inner(r :+ y, xs, ys1)
    }

    inner(Nil, ts1, ts2)
  }

  private def map2[T: Ordering](t1f: Future[Seq[T]], t2f: Future[Seq[T]])(f: (Seq[T], Seq[T]) => Seq[T])(implicit ec: ExecutionContext): Future[Seq[T]] = for {t1 <- t1f; t2 <- t2f} yield f(t1, t2)
}

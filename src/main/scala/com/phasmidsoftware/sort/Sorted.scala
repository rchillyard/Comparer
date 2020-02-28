/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.sort

import com.phasmidsoftware.comparer.{Comparer, ComparerException}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps}

/**
  * This is not really part of the Comparer/Comparison code and should probably not be here.
  *
  * @param ts a sequence of T.
  * @tparam T the underlying type to be sorted.
  */
case class Sorted[T: Comparer](ts: Seq[T]) extends (() => Seq[T]) {

  private val ct = implicitly[Comparer[T]]

  implicit val ordering: Ordering[T] = ct.toOrdering

  def sort(o: Comparer[T]): Sorted[T] = Sorted(ts)(ct orElse o)

  def apply: Seq[T] = ts.sorted

  def async(implicit ec: ExecutionContext): Future[Seq[T]] = Future(apply)

  def parSort(implicit ec: ExecutionContext): Future[Seq[T]] = Sorted.mergeSort(ts)
}

object Sorted {
  def create[T: Ordering](ts: Seq[T]): Sorted[T] = Sorted(ts)(implicitly[Ordering[T]])

  def verify[T: Comparer](xs: Seq[T]): Boolean = xs.zip(xs.tail).forall(z => implicitly[Comparer[T]].<=(z._1)(z._2))

  def parSort[T: Ordering](tst: (Seq[T], Seq[T]))(implicit ec: ExecutionContext): Future[Seq[T]] = map2(Future(tst._1.sorted), Future(tst._2.sorted))(merge)

  def mergeSort[T: Ordering](ts: Seq[T])(implicit ec: ExecutionContext): Future[Seq[T]] = parSort(ts splitAt (ts.length / 2))

  def merge[T: Ordering](ts1: Seq[T], ts2: Seq[T]): Seq[T] = {
    val ordering = implicitly[Ordering[T]]

    @tailrec def inner(r: Seq[T], xs: Seq[T], ys: Seq[T]): Seq[T] = (xs, ys) match {
      case (_, Nil) => r ++ xs
      case (Nil, _) => r ++ ys
      case (x :: xs1, y :: ys1) =>
        if (ordering.lt(x, y)) inner(r :+ x, xs1, ys)
        else inner(r :+ y, xs, ys1)
      case (_, _) => throw ComparerException(s"Unmatched: ($xs, $ys)")
    }

    inner(Nil, ts1, ts2)
  }

  def map2[T: Ordering](t1f: Future[Seq[T]], t2f: Future[Seq[T]])(f: (Seq[T], Seq[T]) => Seq[T])(implicit ec: ExecutionContext): Future[Seq[T]] = for {t1 <- t1f; t2 <- t2f} yield f(t1, t2)

}

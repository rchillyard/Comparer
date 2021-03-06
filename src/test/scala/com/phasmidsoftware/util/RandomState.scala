/*
 * Copyright (c) 2019. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.util

trait RandomState[A] {
  def next: RandomState[A]

  def value: A

  def stream: LazyList[A] = LazyList.cons[A](value, next.stream)

  def map[B](f: A => B): RandomState[B]
}

case class RandomStateJava[A](x: Long)(f: Long => A) extends RandomState[A] {
  def next: RandomState[A] = RandomStateJava[A](new java.util.Random(x).nextLong())(f)

  def value: A = f(x)

  def map[B](g: A => B): RandomState[B] = RandomStateJava(x)(f andThen g)
}

object RandomState {
  def apply(x: Long): RandomState[Long] = RandomStateJava[Long](x)(identity[Long])

  def apply: RandomState[Long] = RandomState(System.currentTimeMillis())
}

# Comparer [![CircleCI](https://circleci.com/gh/rchillyard/Comparer.svg?style=svg)](https://circleci.com/gh/rchillyard/Comparer)

A functional (three-way) comparer

## Introduction

Why do I say that this is a functional comparer? Because _Comparers_ can be composed!

Let's take a look at a typical date comparison using the built-in comparisons provided by Scala
(and, ultimately, Java):

    case class Date(year: Int, month: Int, day: Int) extends Ordered[Date] {
      def compare(that: Date): Int = {
        val cfy = year.compareTo(that.year)
        if (cfy!=0) cfy
        else {
          val cfm = month.compareTo(that.month)
          if (cfm!=0) cfm
          else day.compareTo(that.day)
        }
      }
    }
    
Yes, I know that we could also have used _Ordering_, which would have involved declaring an _Ordering[Date]_
in the companion object of _Date_.
It would, then, have looked a little more similar to the functional version below.
It would have had its own set of complications then.

A typical usage of this in a specification might be:

    val today = Date(2019, 6, 5)
    val tomorrow = Date(2019, 6, 6)
    today.compare(today) shouldBe 0
    today.compare(tomorrow) shouldBe -1
    tomorrow.compare(today) shouldBe 1

Note that the 0, 1 and -1 values almost rise to the level of magic numbers.
They have a significance that is far above their actual values.
Indeed, if you performed the same comparison on an object with Strings, the negative and positive
values could be anything at all.

Now, lets look at the functional way of doing comparisons, using this library:

    case class Date(year: Int, month: Int, day: Int)
    
Note that this is just the same as a previous Date, except that we no longer need to extend _Ordering_.

    object Date {
      implicit val dateComparer: Comparer[Date] = {
        val cf = implicitly[Comparer[Int]]
        cf.snap[Date](_.year) orElse cf.snap[Date](_.month) orElse cf.snap[Date](_.day)
      }
    }

We find an implicit value of a type class for the integer comparer, and we make this a variable called _cf_.  

Actually, we can come up with something rather more elegant than this:

    object Date {
      implicit val dateComparer: Comparer[Date] = Comparer.same[Date] :| (_.year) :| (_.month) :| (_.day)
    }

The _Compare.same_ method simply provides a _Comparer_ of the given type which always evaluates to _Same_.
The _:|_ method composes (using _orElse_) two _Comparers_ where the one on the right is
constructed from an implicitly discovered _Comparer_ of the type yielded by the "lens" function lambda
and which is then snapped by the given lens.

Actually, since the lens functions are all of type Date=>Int, we can do even better:

    object Date {
      implicit val dateComparer: Comparer[DateF] = Comparer(_.year, _.month, _.day)
    }

Now, isn't that a lot more elegant?

The apply method takes a variable list of lens functions, but they must all be of the same type.

Now, we've got the compiler doing some serious work for us.
For each of the lens functions, the compiler will find an implicit Comparer and do apply the lens function to it (via snap).

A typical usage of this in a specification might be:

    val today = Date(2019, 6, 5)
    val tomorrow = Date(2019, 6, 6)
    Comparison(today, today) shouldBe Same
    Comparison(today, tomorrow) shouldBe Less
    Comparison(tomorrow, today) shouldBe More

## API

The chief methods of the API are as follows:

### Comparer

    trait Comparer[T] extends (((T, T)) => Comparison) {
    
      /**
        * Method to convert this Comparer[T] into an Ordering[T] which can then be used for more typical Java/Scala-style comparisons.
        *
        * @return a new Ordering[T].
        */
      def toOrdering: Ordering[T]
    
      /**
        * Methods to yield a Boolean from this Comparer, given a tuple of two Ts.
        *
        * @param tt the tuple of Ts. Try saying that a few times!
        * @return a Boolean which is true if the relationship with respect to first then second is true.
        */
      def >(tt: (T, T)): Boolean
    
      def <(tt: (T, T)): Boolean
    
      def ==(tt: (T, T)): Boolean
    
      def >=(tt: (T, T)): Boolean
    
      def <=(tt: (T, T)): Boolean
    
      def !=(tt: (T, T)): Boolean
    
      /**
        * Method to apply a lens function U=>T to this Comparer[T], resulting in a Comparer[U].
        * The U values that are the inputs to the resulting Comparer, are passed to the lens function to
        * yield a suitable pair of T values which can then be passed into this Comparer.
        *
        * I originally considered this the infamous "unMap" method which is part of an un-monad.
        * Observe that the lens function is U=>T instead of T=>U which would be the parameter of a monadic map method.
        * In case you're wondering, an un-monad is the wrappee as opposed to the wrapper (which would be a monad).
        * However, I think it makes slightly more sense to call this snap because it creates something (kind of like a picture)
        * by using a lens.
        *
        * @param lens a function which takes a U and returns a T.
        * @tparam U the underlying type of the returned Comparer.
        * @return a Comparer[U].
        */
      def snap[U](lens: U => T): Comparer[U]
    
      /**
        * A method to compose this Comparer with another Comparer of a different underlying type.
        *
        * @param uc a Comparer[U].
        * @tparam U the underlying type of uc.
        * @return a Comparer of tuples each comprising a T and a U.
        */
      def compose[U](uc: => Comparer[U]): Comparer[(T, U)]
    
      /**
        * Compose this Comparer with another Comparer of the same underlying type.
        *
        * @param tc the other Comparer (lazily evaluated).
        * @return the result of applying this Comparer unless it yields Same, in which case we invoke the other Comparer.
        */
      def orElse(tc: => Comparer[T]): Comparer[T]
    
      /**
        * A non-monadic map method which maps this Comparer into a different Comparer,
        * but of the same underlying type.
        *
        * @param f the function which takes a Comparison and yields a different Comparison.
        * @return a new Comparer[T].
        */
      def map(f: Comparison => Comparison): Comparer[T]
    
      /**
        * Method to invert the sense of a Comparer.
        *
        * @return a Compare[T] which, given the same tuple of Ts, yields the complementary Comparison to this Comparer.
        */
      def invert: Comparer[T]
    
      /**
        * Method to compose this Comparer[T] with a "lens" function that operates on a T.
        * See, for example, the definition of the Comparer in object DateF (in CompareSpec).
        *
        * The resulting Comparer[T] is formed by using orElse to compose this Comparer[T] with a Comparer[T] that:
        * is formed by using the "lens" function lens to snap (the implicit) comparer (which is a Comparer[U]) into a Comparer[T].
        *
        * This method is used primarily when chaining together several Comparers, each of which is derived from the given function
        * by invoking snap on an implicitly-defined Comparer, with the a specified function.
        * The initial value is typically provided by the "same" method of Comparer's companion object.
        *
        * @param lens a function which takes a T (the underlying type of this and the result) and returns a U.
        * @tparam U the underlying type of the implicit comparer.
        * @return a Comparer[T] which is composed from this and the unmapped form of comparer.
        */
      def :|[U: Comparer](lens: T => U): Comparer[T]
    }
    
    object Comparer {
    
      /**
        * A method to construct a Comparer which always evaluates to Same.
        * This is used, for example, in the apply method following.
        *
        * @tparam T the underlying type of the Comparer.
        * @return a Comparer[T] which always evaluates to Same.
        */
      def same[T]: Comparer[T]
    
      /**
        * Following are the Comparer definitions for the common scalar types.
        */
      implicit val intComparer: Comparer[Int] = Ordering[Int]
      implicit val strComparer: Comparer[String] = Ordering[String]
      implicit val doubleComparer: Comparer[Double] = Ordering[Double]
      implicit val longComparer: Comparer[Long] = Ordering[Long]
      implicit val bigIntComparer: Comparer[BigInt] = Ordering[BigInt]
    
      /**
        * Implicit converter from Ordering[T] to Comparer[T].
        *
        * @param to the Ordering[T] to be converted.
        * @tparam T the underlying type of to and the result.
        * @return a Comparer[T] which has the same intrinsic behavior as "to".
        */
      implicit def convert[T](to: Ordering[T]): Comparer[T]


### Comparison

From the application programmer's perspective, the following methods of Comparison are important:

    sealed trait Comparison extends (() => Option[Boolean]) {
    
      /**
        * Method to eagerly evaluate this Comparison.
        *
        * @return an Option[Boolean].
        */
      def apply(): Option[Boolean]
    
      /**
        * Method to yield logical AND with short-circuit logic.
        *
        * @param c the other Comparison (lazily evaluated).
        * @return a Comparison according to Kleenean logic.
        */
      def &&(c: => Comparison): Comparison
    
      /**
        * Method to yield logical OR with short-circuit logic.
        *
        * @param c the other Comparison (lazily evaluated).
        * @return a Comparison according to Kleenean logic.
        */
      def ||(c: => Comparison): Comparison
    
      /**
        * Method to return the Java-style value of this Comparison.
        *
        * @return if Same then 0 else if Different(true) then -1 else 1
        */
      def toInt: Int
    
      /**
        * Method to compose this with another Comparison.
        * That is to say we yield either this or, in the case that this is Same, a default value of Comparison.
        *
        * @param c the other Comparison (lazily evaluated).
        * @return the composition of this and c.
        */
      def orElse(c: => Comparison): Comparison
    
      /**
        * Method to yield the complementary Comparison to this Comparison, that's to say the result is flipped (i.e. negated).
        *
        * @return Same if this Comparison is Same else the complementary (flipped) value of Different.
        */
      def flip: Comparison
    }
    
    /**
      * Case class which represents a Comparison which is different.
      *
      * @param less true or false. By conventions, we yield a true value when we compare a lesser object with a greater object.
      */
    case class Different(less: Boolean) extends Comparison {
      // methods implemented as appropriate
    }
    
    /**
      * Case class which represents sameness (two objects compare as equal).
      */
    case object Same extends Comparison {
      // methods implemented as appropriate
    }
    
    /**
      * Companion object for Comparison.
      */
    object Comparison {
      /**
        * Different(false)
        */
      val More: Comparison
      /**
        * Different(true)
        */
      val Less: Comparison
    
      /**
        * Method to construct a Comparison from two objects of type T.
        * @param t1 the first T.
        * @param t2 the second T.
        * @param comparer an implicit Comparer[T].
        * @tparam T the type of both t1 and t2, and also the underlying type of the Comparer[T].
        * @return a Comparison, resulting from applying the comparer to the tuple of t1 and t2.
        */
      def apply[T](t1: T, t2: T)(implicit comparer: Comparer[T]): Comparison
    }

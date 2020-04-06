[![Codacy Badge](https://api.codacy.com/project/badge/Grade/9861f150f6e04200b991de30969ea536)](https://app.codacy.com/manual/scalaprof/Comparer?utm_source=github.com&utm_medium=referral&utm_content=rchillyard/Comparer&utm_campaign=Badge_Grade_Settings)
# Comparer [![CircleCI](https://circleci.com/gh/rchillyard/Comparer.svg?style=svg)](https://circleci.com/gh/rchillyard/Comparer)
A functional (three-way) comparer.

## Introduction
Why do I say that this is a functional comparer? Because _Comparers_ can be composed!

### The old object-oriented way
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
    
A typical usage of this in a specification might be:

    val today = Date(2019, 6, 5)
    val tomorrow = Date(2019, 6, 6)
    today.compareTo(today) shouldBe 0
    today.compareTo(tomorrow) shouldBe -1
    tomorrow.compareTo(today) shouldBe 1

Yes, I know that we could also have used _Ordering_, which would have involved declaring an _Ordering of Date_
in the companion object of _Date_.

It would look like this:

      object Date {
        trait OrderingDate extends Ordering[Date] {
          def compare(d1: Date, d2: Date): Int = {
            val cfy = d1.year.compareTo(d2.year)
            if (cfy != 0) cfy
            else {
              val cfm = d1.month.compareTo(d2.month)
              if (cfm != 0) cfm
              else d1.day.compareTo(d2.day)
            }
          }
        }
        implicit object OrderingDate extends OrderingDate
      }
    
And we wouldn't need the mixin of _Ordered of Date_ in the case class any more.

A typical usage of this in a specification might be:

    val today = Date(2019, 6, 5)
    val tomorrow = Date(2019, 6, 6)
    val ordering = implicitly[Ordering[DateJ]]
    ordering.compare(today, today) shouldBe 0
    ordering.compare(today, tomorrow) shouldBe -1
    ordering.compare(tomorrow, today) shouldBe 1

This looks a little more like the functional version below.
But the _compare_ method itself is still very inelegant with all of those temporary variables.

Note that the 0, 1 and -1 values almost rise to the level of magic numbers.
They have a significance that is far above their actual values.
Indeed, if you performed the same comparison on an object with _String_s, the negative and positive
values could be anything at all.

### The new functional way
Now, let's look at the functional way of doing comparisons, using the _Comparer_ library:

    case class Date(year: Int, month: Int, day: Int)
    
Note that this (the case class) is just the same as the previous _Date_.

    object Date {
      implicit val dateComparer: Comparer[Date] = {
        val cf = implicitly[Comparer[Int]]
        cf.snap[Date](_.year) orElse cf.snap[Date](_.month) orElse cf.snap[Date](_.day)
      }
    }

We find an implicit value of a type class for the integer comparer, and we make this a variable called _cf_.
The _snap_ method takes a "lens" function as its parameter and transforms the _Comparer of Int_ into a _Comparer of Date_.

Actually, we can come up with something rather more elegant than this:

    object Date {
      implicit val dateComparer: Comparer[Date] = Comparer.same[Date] :| (_.year) :| (_.month) :| (_.day)
    }

The _Compare.same_ method simply provides a _Comparer_ of the given type which always evaluates to _Same_.
The _:|_ method composes (using _orElse_) two _Comparers_ where the one on the right is
constructed from an implicitly discovered _Comparer_ of the type yielded by the lens function lambda
and which is then snapped by the given lens.

There's also a _:|!_ method which works the same except that it invokes the _orElseNot_ method which flips
the sense of the _Comparer_ formed from the lens function.

Actually, since in this case the lens functions are all of type _Date=>Int_ and all of the same sense, we can do even better:

    object Date {
      implicit val dateComparer: Comparer[Date] = Comparer(_.year, _.month, _.day)
    }

Now, isn't that a lot more elegant?
The _apply_ method takes a variable list of lens functions, but they must all be of the same type.

Now, we've got the compiler doing some serious work for us.
For each of the lens functions, the compiler will find an implicit _Comparer_ and apply the lens function to it (via _snap_).

A typical usage of this in a specification might be:

    val today = Date(2019, 6, 5)
    val tomorrow = Date(2019, 6, 6)
    Compare(today, today) shouldBe Same
    Compare(today, tomorrow) shouldBe Less
    Compare(tomorrow, today) shouldBe More

Well, of course, that's not quite it. We can do even better:

    object MyComparers extends Comparers {
      val comparer: Comparer[Date] = comparer3(Date)
    }
    import MyComparers._
    Compare(tomorrow, today) shouldBe More

This time, we didn't have to spell out how to compare the various elements of the _Date_ case class.
The compiler figured it out for us using the magic of type inference.
Notice that we simply pass in the _Date.apply_ method to the _comparer3_ method.
But, inside _comparer3_, we never actually have to invoke that _apply_ method.
The work is all done by the compiler.

Now, we've really got the compiler doing some serious work for us!

## API

The chief methods of the API are as follows:

### Comparer

Comparer extends the (curried) function _T => T => Comparison_.
This makes sense since the T values being compared are not related and so don't naturally form
part of a tuple.
It also allows the use of a partially applied comparer.
Note that, when curried parameters are used, we compare the outer (last) parameter with the inner,
whereas when tupled parameters are used, it is conventional to compare the first parameter the second.

    trait Comparer[T] extends (T => T => Comparison) {
    
      /**
        * Method to convert this Comparer[T] into an Ordering[T] which can then be used for more typical Java/Scala-style comparisons.
        *
        * @return a new Ordering[T].
        */
      def toOrdering: Ordering[T]
    
      /**
        * The following methods are defined both in tupled and curried signatures (only the tupled forms are shown here).
        * When you use these tupled forms, the compiler doesn't need an extra set of parentheses.
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
        * There is also a similar method orElseNot which flips the sense of tc.
        *
        * @param tc the other Comparer (lazily evaluated).
        * @return the result of applying this Comparer unless it yields Same, in which case we invoke the other Comparer.
        */
      def orElse(tc: => Comparer[T]): Comparer[T]
    
      /**
        * A non-monadic transform method which transforms this Comparer into a different Comparer,
        * but of the same underlying type. NOTE: this was formerly called map.
        *
        * @param f the function which takes a Comparison and yields a different Comparison.
        * @return a new Comparer[T].
        */
      def transform(f: Comparison => Comparison): Comparer[T]
    
      /**
        * Method to invert the sense of a Comparer.
        *
        * @return a Compare[T] which, given the same tuple of Ts, yields the complementary Comparison to this Comparer.
        */
      def invert: Comparer[T]
    
      /**
        * Method to compose this Comparer[T] with a "lens" function that operates on a T.
        * See, for example, the definition of the Comparer in object Date (in CompareSpec).
        *
        * The resulting Comparer[T] is formed by using orElse to compose this Comparer[T] with a Comparer[T] that:
        * is formed by using the "lens" function lens to snap (the implicit) comparer (which is a Comparer[U]) into a Comparer[T].
        *
        * This method is used primarily when chaining together several Comparers, each of which is derived from the given function
        * by invoking snap on an implicitly-defined Comparer, with the a specified function.
        * The initial value is typically provided by the "same" method of Comparer's companion object.
        * 
        * There is a similar method :|! which invokes orElseNot instead of orElse.
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
      implicit val booleanComparer: Comparer[Boolean] = Ordering[Boolean]
      implicit val stringComparer: Comparer[String] = Ordering[String]
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
      
    object Compare {
      /**
        * Method to construct a Comparison from two objects of type T (tupled form).
        * The sense of the result of this comparison is the same as,
        * for example, Double.compare(t1, t2).
        *
        * @param t1 the first T.
        * @param t2 the second T.
        * @tparam T the type of both t1 and t2, such type providing implicit evidence of a Comparer[T].
        * @return a Comparison, resulting from applying the comparer to the tuple of t1 and t2.
        */
      def apply[T: Comparer](t1: T, t2: T): Comparison = Comparison(t2)(t1)
    }


### Comparison

From the application programmer's perspective, the following methods of _Comparison_ are important:

    sealed trait Comparison extends (() => Kleenean) {
    
      /**
        * Method to eagerly evaluate this Comparison.
        *
        * @return a Kleenean.
        */
      def apply(): Kleenean
    
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
        * Method to construct a Comparison from two objects of type T (curried form).
        * NOTE: the sense of the result will be similar to invoking, for example, Double.compare(t1, t2).
        * @param t2 the inner T.
        * @param t1 the outer T.
        * @param comparer an implicit Comparer[T].
        * @tparam T the type of both t1 and t2, which type must also provide implicit evidence of type Comparer[T].
        * @return a Comparison, resulting from applying the comparer to the tuple of t1 and t2.
        */
      def apply[T](t2: T)(t1: T)(implicit comparer: Comparer[T]): Comparison
    }
    
### Kleenean
The result of evaluating a _Comparison_ is a _Kleenean_: a three-valued logic type.
_Kleenean_ evaluates to an _Option of Boolean_.

### Comparers
This trait provides methods to create a _Comparer_ for a case class (or other _Product_).
It assumes that the parameters of the case class are in order of significance: most to least.
All the programmer needs to do is to create an object which extends _Comparers_, and
create a variable using the _comparerN_ method with the appropriate value of _N_ (according to the number of parameters).

    case class Composite(i: Int, s: String)
    object MyComparers extends Comparers {
      val compositeComparer: Comparer[Composite] = comparer2(Composite)
    }

You may have to give the name of the _apply_ function explicitly in some cases (for example, when
there is a companion object).

There are additionally, implicit methods which will create a _Comparer_ for a wrapper of a type.
Currently defined are:
 
* _comparerIterable_: _Comparer[Iterable[T]]_
* _comparerSeq_: _Comparer[Seq[T]]_
* _comparerList_: _Comparer[List[T]]_
* _comparerArray_: _Comparer[Array[T]]_
* _comparerOpt_: _Comparer[Option[T]]_
* _comparerTry_: _Comparer[Try[T]]_
* _comparerEither: Comparer[Either[_,T]]_

So, if your case class happens to include an iterable (sequence or list), array, optional, try or "either" types, you can still use
one of the _comparerN_ methods and the types will be handled.
You don't have to do anything in your code (other than importing the implicits) to get the benefit of this feature.
The _comparerN_ methods are implemented up through _N_ = 11.

What if the case class you want to compare doesn't have its fields in descending order of significance?
For example, you might be using a _Date_ class where the fields are laid out: day, month, year.
There is a set of function manipulation methods in _generic.Functional_.
For instance, for the situation mentioned, you can simply write something like the following:

        case class DateJ(day: Int, month: Int, year: Int)
        object MyComparers extends Comparers {
          import Functional._
          val comparer: Comparer[DateJ] = comparer3(invert3(DateJ))
        }
 
If you have a more complex situation, such as only needing to invert the first two parameters, please see _ComparersSpec_.
### Colophon

This project has 100% coverage so it makes sense to resolve any doubtful points about usage by consulting
the specifications (i.e. unit tests).

### Versioning
Version 1.0.1 introduces the notion of curried parameters for the internal workings and for some of the
application-oriented methods.
This is a more functional approach and gives us the invaluable option of easily creating partially applied functions.

Version 1.0.2 introduces a _Comparers_ trait which allows a programmer easily to get a comparer
for a case class, assuming that the fields are in order from most to least significant.

Version 1.0.3 adds compareIterable, compareList, compareArray, compareTry, compareEither and compare7 thru compare10. Also _Comparer of Boolean_.

Version 1.0.4 adds Functional module and provides support for Scala 2.13

Version 1.0.5 introduced the Kleenean trait and has Comparison return it with apply().
Otherwise, no logic changes.

Version 1.0.6 merged 1.0.5 with master branch.

Version 1.0.7 improved usability slightly by moving compare method of Comparison into apply method of (new) Compare object.

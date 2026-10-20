![Sonatype Central](https://maven-badges.sml.io/sonatype-central/com.phasmidsoftware/comparer_3/badge.svg?color=blue)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/9861f150f6e04200b991de30969ea536)](https://app.codacy.com/gh/rchillyard/Comparer/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/rchillyard/Comparer/tree/master.svg?style=shield)](https://dl.circleci.com/status-badge/redirect/gh/rchillyard/Comparer/tree/master)
![GitHub Top Languages](https://img.shields.io/github/languages/top/rchillyard/Comparer)
![GitHub](https://img.shields.io/github/license/rchillyard/Comparer)
![GitHub last commit](https://img.shields.io/github/last-commit/rchillyard/Comparer)
![GitHub issues](https://img.shields.io/github/issues-raw/rchillyard/Comparer)
![GitHub issues by-label](https://img.shields.io/github/issues/rchillyard/Comparer/bug)
[![API Docs](https://javadoc.io/badge2/com.phasmidsoftware/comparer_3/javadoc.svg)](https://javadoc.io/doc/com.phasmidsoftware/comparer_3)

# Comparer: a functional three-way comparer.

## Making _Comparer_ a dependency
If you're using sbt, then include the following line (see the badge above for the current version):

    libraryDependencies += "com.phasmidsoftware" %% "comparer" % "2.0.0"
    
Comparer 2.x is for Scala 3 only, and can be used from Scala 3.3 or later.
If you are using Scala 2.13, then use version 1.1.0 (see [Migrating from 1.x](#migrating-from-1x)).

Full Scaladoc is available via [javadoc.io](https://javadoc.io/doc/com.phasmidsoftware/comparer_3).

## Introduction
Why do I say that this is a functional comparer? Because _Comparers_ can be composed!
And it's three-way, like the rather primitive Java mechanism, because _Comparer_ distinguishes between less than, equals, and greater than.

### The "old" object-oriented way
Let's take a look at a typical date comparison using the built-in comparisons provided by Scala
(and, ultimately, Java):

    case class Date(year: Int, month: Int, day: Int) extends Ordered[Date]:
      def compare(that: Date): Int =
        val cfy = year.compareTo(that.year)
        if cfy != 0 then cfy
        else
          val cfm = month.compareTo(that.month)
          if cfm != 0 then cfm
          else day.compareTo(that.day)
    
A typical usage of this in a specification might be:

    val today = Date(2019, 6, 5)
    val tomorrow = Date(2019, 6, 6)
    today.compareTo(today) shouldBe 0
    today.compareTo(tomorrow) shouldBe -1
    tomorrow.compareTo(today) shouldBe 1

Yes, I know that we could also have used _Ordering_, which would have involved declaring an _Ordering of Date_
in the companion object of _Date_.

It would look like this:

    object Date:
      given Ordering[Date] with
        def compare(d1: Date, d2: Date): Int =
          val cfy = d1.year.compareTo(d2.year)
          if cfy != 0 then cfy
          else
            val cfm = d1.month.compareTo(d2.month)
            if cfm != 0 then cfm
            else d1.day.compareTo(d2.day)
    
And we wouldn't need the mixin of _Ordered of Date_ in the case class any more.

A typical usage of this in a specification might be:

    val today = Date(2019, 6, 5)
    val tomorrow = Date(2019, 6, 6)
    val ordering = summon[Ordering[Date]]
    ordering.compare(today, today) shouldBe 0
    ordering.compare(today, tomorrow) shouldBe -1
    ordering.compare(tomorrow, today) shouldBe 1

This looks a little more like the functional version below.
But the _compare_ method itself is still very inelegant with all of those temporary variables.

Note that the 0, 1 and -1 values almost rise to the level of magic numbers.
They have a significance that is far above their actual values.
Indeed, if you performed the same comparison on an object with _String_s, the negative and positive
values could be anything at all.

### The "new" functional way
Now, let's look at the functional way of doing comparisons, using the _Comparer_ library:

    case class Date(year: Int, month: Int, day: Int)
    
Note that this (the case class) is just the same as the previous _Date_.

    object Date:
      given Comparer[Date] =
        val cf = summon[Comparer[Int]]
        cf.snap[Date](_.year) orElse cf.snap[Date](_.month) orElse cf.snap[Date](_.day)

We summon the given type class instance for the integer comparer, and we make this a variable called _cf_.
The _snap_ method takes a "lens" function as its parameter and transforms the _Comparer of Int_ into a _Comparer of Date_.

Actually, we can come up with something rather more elegant than this:

    object Date:
      given Comparer[Date] = Comparer.same[Date] :| (_.year) :| (_.month) :| (_.day)

The _Comparer.same_ method simply provides a _Comparer_ of the given type which always evaluates to _Same_.
The _:|_ method composes (using _orElse_) two _Comparers_ where the one on the right is
constructed from the given _Comparer_ of the type yielded by the lens function lambda
and which is then snapped by the given lens.

There's also a _:|!_ method which works the same except that it invokes the _orElseNot_ method which flips
the sense of the _Comparer_ formed from the lens function.

Actually, since in this case the lens functions are all of type _Date=>Int_ and all of the same sense, we can do even better:

    object Date:
      given Comparer[Date] = Comparer(_.year, _.month, _.day)

Now, isn't that a lot more elegant?
The _apply_ method takes a variable list of lens functions, but they must all be of the same type.

Now, we've got the compiler doing some serious work for us.
For each of the lens functions, the compiler will find the given _Comparer_ and apply the lens function to it (via _snap_).

A typical usage of this in a specification might be:

    val today = Date(2019, 6, 5)
    val tomorrow = Date(2019, 6, 6)
    Compare(today, today) shouldBe Same
    Compare(today, tomorrow) shouldBe Less
    Compare(tomorrow, today) shouldBe More

Well, of course, that's not quite it. We can do even better:

    case class Date(year: Int, month: Int, day: Int) derives Comparer

    Compare(tomorrow, today) shouldBe More

This time, we didn't have to spell out how to compare the various elements of the _Date_ case class.
The compiler derives the _Comparer_ for us, from the given _Comparers_ of the field types.
In this form of comparison, the precedence order is fixed by the order of the fields of the case class.
And, furthermore, the sense is always fixed to be the natural ordering.
If you need to reverse the ordering for a field, or change the precedence, then you will have to use one of the previous
methods (see, for example, the :| operator).

Now, we've really got the compiler doing some serious work for us!

## API

This is a summary of the public API. The Scaladoc (see the badge above) has the details.

### Comparer

_Comparer_ extends the curried function _T => T => Comparison_.
That makes sense, since the two _T_ values being compared are unrelated and so don't naturally form a tuple,
and it allows a partially applied comparer.
With the curried form, the outer (second) parameter is compared with the inner (first) one:
_comparer(1)(2)_ is _More_, because 2 is more than 1.
With the tupled forms, the first parameter is compared with the second, as usual:
_comparer.compare(1, 2)_ is _Less_.

    trait Comparer[T] extends (T => T => Comparison):

      // Comparing.
      def compare(tt: (T, T)): Comparison      // compares tt._1 with tt._2
      def tupled(t1: T, t2: T): Comparison     // compares t1 with t2
      def toOrdering: Ordering[T]              // for use with the Scala and Java libraries

      // Boolean tests, each in a tupled and a curried form:
      // comparer.<((1, 2)) is true (1 is less than 2), while comparer.<(1) is the predicate "is less than 1".
      def <(tt: (T, T)): Boolean               // also <(t1: T)(t2: T)
      def <=(tt: (T, T)): Boolean              // also <=(t1: T)(t2: T)
      def >(tt: (T, T)): Boolean               // also >(t1: T)(t2: T)
      def >=(tt: (T, T)): Boolean              // also >=(t1: T)(t2: T)
      def ==(tt: (T, T)): Boolean              // also ==(t1: T)(t2: T)
      def !=(tt: (T, T)): Boolean              // also !=(t1: T)(t2: T)

      // Building new Comparers.
      infix def snap[U](lens: U => T): Comparer[U]                // compares Us by the T that lens yields
      def :|[U: Comparer](lens: T => U): Comparer[T]              // this, then the given Comparer[U] applied via lens
      def :|![U: Comparer](lens: T => U): Comparer[T]             // the same, but with the second Comparer inverted
      infix def orElse(tc: => Comparer[T]): Comparer[T]           // this, then tc where this yields Same
      infix def orElseNot(tc: => Comparer[T]): Comparer[T]        // this, then tc inverted where this yields Same
      infix def compose[U](uc: => Comparer[U]): Comparer[(T, U)]  // compares pairs: by this, then by uc
      def transform(f: Comparison => Comparison): Comparer[T]     // applies f to every result (formerly map)
      def invert: Comparer[T]                                     // reverses the sense of this Comparer

    object Comparer:

      // Creating Comparers.
      def same[T]: Comparer[T]                                    // always Same: the usual start of a :| chain
      def different[T](less: Boolean): Comparer[T]                // always Different(less)
      def apply[T, U: Comparer](lenses: (T => U)*): Comparer[T]   // same :| lens1 :| lens2 ..., for lenses of one type
      def create[T](comparers: Comparer[T]*): Comparer[T]         // comparer1 orElse comparer2 ...
      def comparer[T, P: Comparer](lens: T => P): Comparer[T]     // the given Comparer[P], snapped by lens
      def fromOrdering[T](to: Ordering[T]): Comparer[T]           // a Comparer with the behavior of an Ordering
      inline def derived[T](using m: Mirror.Of[T]): Comparer[T]   // what makes "derives Comparer" work (see below)

      // Comparing Iterables.
      def comparePrefixes[T: Comparer](ts1: Iterable[T], ts2: Iterable[T]): Comparison  // as far as the shorter one
      def compareAll[T: Comparer](ts1: Iterable[T], ts2: Iterable[T]): Comparison       // ...then the longer is More

      // Given instances: scalar types.
      given unitComparer: Comparer[Unit]
      given booleanComparer: Comparer[Boolean]
      given byteComparer: Comparer[Byte]
      given charComparer: Comparer[Char]
      given shortComparer: Comparer[Short]
      given intComparer: Comparer[Int]
      given longComparer: Comparer[Long]
      given floatComparer: Comparer[Float]        // total ordering
      given doubleComparer: Comparer[Double]      // total ordering
      given bigIntComparer: Comparer[BigInt]
      given bigDecimalComparer: Comparer[BigDecimal]
      given stringComparer: Comparer[String]

      // Given instances: tuples of any size, compared element by element.
      given emptyTupleComparer: Comparer[EmptyTuple]
      given tupleComparer[H, R <: Tuple](using hc: => Comparer[H], rc: => Comparer[R]): Comparer[H *: R]

      // Given instances: "wrapper" types (see Derived comparers, below, for how None, Failure and Left compare).
      given comparerIterable[T: Comparer]: Comparer[Iterable[T]]
      given comparerSeq[T: Comparer]: Comparer[Seq[T]]
      given comparerList[T: Comparer]: Comparer[List[T]]
      given comparerArray[T: Comparer]: Comparer[Array[T]]
      given comparerOpt[T: Comparer]: Comparer[Option[T]]
      given comparerTry[T: Comparer]: Comparer[Try[T]]
      given comparerEither[L, R: Comparer]: Comparer[Either[L, R]]

    object Compare:
      def apply[T: Comparer](t1: T, t2: T): Comparison            // compares t1 with t2, like Double.compare(t1, t2)

All of these given instances are in the companion object of _Comparer_, so you never need to import them.

### Comparison

    enum Comparison:
      case Same
      case Different(less: Boolean)       // less is true if the first value is less than the second

      def toInt: Int                      // -1 (Less), 0 (Same) or 1 (More), as in Java
      def flip: Comparison                // Less and More swap; Same stays Same
      infix def orElse(c: => Comparison): Comparison   // this, or c if this is Same

      // Three-valued logic, in which More is true, Less is false and Same is indeterminate.
      def &(c: Comparison): Comparison    // AND
      def |(c: Comparison): Comparison    // OR
      def &&(c: => Comparison): Comparison  // AND, evaluating c only when needed
      def ||(c: => Comparison): Comparison  // OR, evaluating c only when needed
      def toKleenean: Kleenean            // More is Truth(true), Less is Truth(false), Same is Maybe

    object Comparison:
      val Less: Comparison                // Different(true)
      val More: Comparison                // Different(false)
      def apply(b: Boolean): Comparison           // Different(b): b says whether it is less
      def apply(bo: Option[Boolean]): Comparison  // Some(b) is Different(b); None is Same
      def apply(k: Kleenean): Comparison          // the inverse of toKleenean
      def convert(x: Int): Comparison             // from a Java-style comparison result
      def apply[T: Comparer](t2: T)(t1: T): Comparison  // curried: compares t1 with t2

_Same_, _Different_, _Less_ and _More_ are exported at the package level,
so `import com.phasmidsoftware.comparer.*` lets you refer to them without the _Comparison_ prefix.

### Kleenean
A _Comparison_ is itself a form of three-valued logic, in which _More_ is true, _Less_ is false and _Same_ is indeterminate:
that is the sense of its _&_, _|_, _&&_, _||_ and _toInt_ methods.
Its _toKleenean_ method converts it into the equivalent _Kleenean_, a general three-valued logic type,
such that the logical operators give the same results either side of the conversion.

    enum Kleenean:
      case Maybe
      case Truth(b: Boolean)

      def toOption: Option[Boolean]       // None for Maybe, otherwise Some(b)
      def toInt: Int                      // -1, 0 or 1
      def |(k: => Kleenean): Kleenean     // OR (evaluates k only when needed)
      def &(k: => Kleenean): Kleenean     // AND (evaluates k only when needed)
      def ! : Kleenean                    // NOT
      def getOrElse(x: => Boolean): Boolean  // b, or x if this is Maybe

    object Kleenean:
      def apply(b: Boolean): Kleenean     // Truth(b)
      def apply(x: Int): Kleenean         // Maybe if x is 0, otherwise Truth(x > 0)

_Maybe_ and _Truth_ are also exported at the package level.

### Derived comparers
You can derive a _Comparer_ for a case class (or other _Product_) simply by adding _derives Comparer_.
It assumes that the parameters of the case class are in order of significance: most to least.

    case class Composite(i: Int, s: String) derives Comparer

Each field type must itself have a given _Comparer_.
As well as the scalar types, there are given _Comparers_ for tuples (of any size) and for these "wrapper" types,
so long as the type they wrap has a given _Comparer_:
 
* _comparerIterable_: _Comparer[Iterable[T]]_
* _comparerSeq_: _Comparer[Seq[T]]_
* _comparerList_: _Comparer[List[T]]_
* _comparerArray_: _Comparer[Array[T]]_
* _comparerOpt_: _Comparer[Option[T]]_
* _comparerTry_: _Comparer[Try[T]]_
* _comparerEither_: _Comparer[Either[L, T]]_

So, if your case class happens to include a tuple, an iterable (sequence or list), array, optional, try or "either" types,
you can still derive its _Comparer_ and the types will be handled.
Note two conventions of these _Comparers_: iterables (and arrays) are compared only as far as the shorter of the two
(use _Comparer.compareAll_ if the longer one should count as greater);
and any comparison involving _None_, a _Failure_ or a _Left_ yields _Same_.
There is no limit on the number of fields.

You can also derive a _Comparer_ for an enum or a sealed trait.
Values of different cases are compared by the order in which the cases are declared;
values of the same case are compared by that case's own given _Comparer_ if it has one, otherwise by its fields:

    enum Suit derives Comparer:
      case Clubs, Diamonds, Hearts, Spades

    enum Shape derives Comparer:
      case Circle(radius: Double)
      case Rectangle(width: Double, height: Double)

    sealed trait Contact derives Comparer
    case class Email(address: String) extends Contact
    case class Phone(number: Long) extends Contact

    Compare(Suit.Clubs, Suit.Spades) shouldBe Less
    Compare(Shape.Circle(10), Shape.Rectangle(1, 1)) shouldBe Less
    Compare[Contact](Email("a@x"), Phone(1)) shouldBe Less

Note the type parameter in the last line: the case classes of a sealed trait keep their own types,
so we tell _Compare_ to use the _Comparer_ of _Contact_ (enum cases don't need this).
Recursive types, such as a tree whose nodes contain trees, are also supported.

What if the case class you want to compare doesn't have its fields in descending order of significance?
For example, you might be using a _Date_ class where the fields are laid out: day, month, year.
Then, instead of deriving the _Comparer_, list the fields in the order you want:

    case class DateJ(day: Int, month: Int, year: Int)
    object DateJ:
      given Comparer[DateJ] = Comparer.same[DateJ] :| (_.year) :| (_.month) :| (_.day)

## Migrating from 1.x
Version 2.0.0 is for Scala 3 only, and some of the API has changed:

| 1.x | 2.0 |
|---|---|
| `object MyComparers extends Comparers { val c = comparer3(Date.apply) }` | `case class Date(...) derives Comparer` |
| `comparerN` with fields in a different order (using `Functional.invertN`) | `Comparer.same[T] :| (_.field) :| ...` |
| `extends Comparers` to get `comparerOpt`, `comparerSeq`, etc. | Nothing: they are given in `Comparer`'s companion |
| `Comparers.comparePrefixes`, `compareAll` | `Comparer.comparePrefixes`, `Comparer.compareAll` |
| `tuple2` thru `tuple9` | `tupleComparer`, for tuples of any size |
| `val c: Comparer[Int] = Ordering[Int]` (implicit conversion), or `Comparer.convert(ordering)` | `val c = Comparer.fromOrdering(Ordering[Int])` |
| `Comparison.convert(x)` as an implicit conversion | `Comparison.convert(x)`, called explicitly |
| `implicit val c: Comparer[T] = ...` | `given Comparer[T] = ...` (`implicit val` still works) |
| `Sorted.mergeSort(ts)` with an implicit `Threads` | `Sorted.mergeSort(ts)` or `Sorted.mergeSort(ts, threads)` |
| `comparison()`, which yielded a `Kleenean` in which `Truth(true)` meant `Less` | `comparison.toKleenean`, in which `Truth(true)` means `More` (see [Kleenean](#kleenean)) |
| `Comparison(Truth(true))` was `Less` | `Comparison(Truth(true))` is `More` (the inverse of `toKleenean`) |
| `kleenean()` | `kleenean.toOption` |
| `import com.phasmidsoftware.comparer.Comparison.{Less, More}` | No longer needed (but still works): they are exported at the package level |
| `Thunk`, `Predicate`, `Sorted.Threads` (type aliases) | Removed |
| `TupleWrangler.strip` | Removed |

If you need Scala 2.13, use version 1.1.0, which is cross-built for Scala 2.13 and Scala 3.

### Colophon
The specifications (i.e. unit tests) cover every statement and every branch of the code (as of version 2.0.0),
so it makes sense to resolve any doubtful points about usage by consulting them.
To measure the coverage with sbt, run `sbt clean coverage test coverageReport`
and open _target/scala-3.3.8/scoverage-report/index.html_.
### Versioning
For the current version, see the badge at the top of the README page.

Version 1.0.1 introduces the notion of curried parameters for the internal workings and for some of the
application-oriented methods.
This is a more functional approach and gives us the invaluable option of easily creating partially applied functions.

Version 1.0.2 introduces a _Comparers_ trait which allows a programmer easily to get a comparer
for a case class, assuming that the fields are in order from most to least significant.

Version 1.0.3 adds comparerIterable, comparerList, comparerArray, comparerTry, comparerEither and comparer7 thru comparer10. Also _Comparer of Boolean_.

Version 1.0.4 adds Functional module and provides support for Scala 2.13

Version 1.0.5 introduced the Kleenean trait and has Comparison return it with apply().
Otherwise, no logic changes.

Version 1.0.6 merged 1.0.5 with master branch.

Version 1.0.7 improved usability slightly by moving compare method of Comparison into apply method of (new) Compare object.

Version 1.0.8 improved support for comparing tuples.

Version 1.1.0 adds Scala 3 support: Comparer is now cross-built for Scala 3 and Scala 2.13. No logic changes.

Version 2.0.0 is for Scala 3 only (built with Scala 3.3, so usable from 3.3 or later), and adopts Scala 3 idioms throughout:
_derives Comparer_ for case classes, enums and sealed traits, a single given _Comparer_ for tuples of any size,
given instances in place of implicits, and enums for _Comparison_ and _Kleenean_
(which are no longer functions: use _toKleenean_ and _toOption_).
See [Migrating from 1.x](#migrating-from-1x).

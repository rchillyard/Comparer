# Comparer
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

/*
 * Copyright (c) 2026. Phasmid Software. Comparer: functional comparison library.
 */

package com.phasmidsoftware.comparer

import com.phasmidsoftware.comparer.Comparison.{Less, More}
import com.phasmidsoftware.sort.Sorted
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Specification of Comparers derived for enums and sealed traits (sum types).
  * Comparers derived for case classes are specified in ComparersSpec.
  */
class DerivedSpec extends AnyFlatSpec with Matchers:

  behavior of "a derived Comparer for an enum"

  it should "compare simple cases in the order they are declared" in:
    import Suit.*
    Compare(Clubs, Diamonds) shouldBe Less
    Compare(Spades, Hearts) shouldBe More
    Compare(Hearts, Hearts) shouldBe Same
    Sorted(List(Spades, Clubs, Hearts, Diamonds))() shouldBe List(Clubs, Diamonds, Hearts, Spades)

  it should "compare cases with fields by case, then by fields" in:
    import Shape.*
    Compare(Circle(10), Rectangle(1, 1)) shouldBe Less
    Compare(Rectangle(1, 1), Circle(10)) shouldBe More
    Compare(Circle(1), Circle(2)) shouldBe Less
    Compare(Rectangle(2, 1), Rectangle(1, 5)) shouldBe More
    Compare(Rectangle(2, 1), Rectangle(2, 5)) shouldBe Less
    Compare(Point, Circle(1)) shouldBe More
    Compare(Point, Point) shouldBe Same

  it should "use the given Comparer of a case if it has one" in:
    import Priority.*
    // Custom's own Comparer orders by descending level.
    Compare(Custom(1), Custom(2)) shouldBe More
    Compare(Low, Custom(1)) shouldBe Less

  behavior of "a derived Comparer for a sealed trait"

  // NOTE: unlike enum cases, the case classes of a sealed trait keep their own types,
  // so we name the trait (Comparer is invariant, so there is no Comparer[Email] as such).
  it should "compare case classes and case objects by declaration order, then by fields" in:
    Compare[Contact](Email("a@x"), Phone(1)) shouldBe Less
    Compare[Contact](Phone(1), NoContact) shouldBe Less
    Compare[Contact](NoContact, Email("a@x")) shouldBe More
    Compare[Contact](Email("a@x"), Email("b@x")) shouldBe Less
    Compare[Contact](Phone(2), Phone(1)) shouldBe More
    Compare[Contact](NoContact, NoContact) shouldBe Same

  it should "compare a nested hierarchy" in:
    // Vehicle's cases are Car (itself sealed, with cases Sedan and Coupe) and Bike.
    Compare[Vehicle](Sedan(4), Coupe(2)) shouldBe Less
    Compare[Vehicle](Coupe(2), Bike(true)) shouldBe Less
    Compare[Vehicle](Sedan(5), Sedan(4)) shouldBe More

  behavior of "a derived Comparer for a recursive type"

  it should "compare trees" in:
    import Tree.*
    val small = Node(Leaf, 1, Leaf)
    val big = Node(Node(Leaf, 0, Leaf), 1, Leaf)
    Compare(Leaf, small) shouldBe Less
    Compare(small, big) shouldBe Less
    Compare(big, big) shouldBe Same
    Compare(Node(Leaf, 1, small), Node(Leaf, 1, big)) shouldBe Less

  behavior of "a derived Comparer for a case class with an enum field"

  it should "use the enum's derived Comparer for that field" in:
    Compare(Card(10, Suit.Spades), Card(10, Suit.Hearts)) shouldBe More
    Compare(Card(2, Suit.Spades), Card(10, Suit.Clubs)) shouldBe Less

enum Suit derives Comparer:
  case Clubs, Diamonds, Hearts, Spades

enum Shape derives Comparer:
  case Circle(radius: Double)
  case Rectangle(width: Double, height: Double)
  case Point

enum Priority derives Comparer:
  case Low
  case Custom(level: Int)

object Priority:
  given Comparer[Priority.Custom] = Comparer.same[Priority.Custom] :|! (_.level)

sealed trait Contact derives Comparer
case class Email(address: String) extends Contact
case class Phone(number: Long) extends Contact
case object NoContact extends Contact

sealed trait Vehicle derives Comparer
sealed trait Car extends Vehicle
case class Sedan(doors: Int) extends Car
case class Coupe(doors: Int) extends Car
case class Bike(electric: Boolean) extends Vehicle

enum Tree derives Comparer:
  case Leaf
  case Node(left: Tree, value: Int, right: Tree)

case class Card(rank: Int, suit: Suit) derives Comparer

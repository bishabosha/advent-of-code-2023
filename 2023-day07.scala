package day07

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.r
import scala.math.Ordering.Implicits.*

enum Card:
  case A, K, Q, J, T, R9, R8, R7, R6, R5, R4, R3, R2

object Card:
  def unapply(card: Char): Option[Card] =
    "AKQJT98765432".indexOf(card) match
      case -1 => None
      case i => Some(Card.fromOrdinal(i))

type Hand = (cards: Seq[Card], bid: Int)
type HandGrouping = Seq[Card] => Seq[Int]

def parse(input: String): Seq[Hand] =
  val r"${r"$cardss $scores%d"}...(\n)" = input: @unchecked
  val cardss0 = cardss.map: cards =>
    cards.map[Card]:
      case Card(c) => c
  cardss0.zip(scores)

def groupBasic(cards: Seq[Card]): Seq[Int] =
  given Ordering[Int] = Ordering.Int.reverse // largest first
  cards.groupBy(identity).values.map(_.size).toSeq.sorted

def groupWithJoker(cards: Seq[Card]): Seq[Int] =
  val noJ = cards.filterNot(_ == Card.J)
  val jokerCount = cards.size - noJ.size
  groupBasic(noJ) match
    case largest +: rest =>
      // jokers merge with the best group
      (largest + jokerCount) +: rest
    case _ =>
      // all jokers
      Seq(jokerCount)

def orderHands(grouping: HandGrouping)(using Ordering[Card]): Ordering[Hand] =
  def score(cards: Seq[Card]): Int = grouping(cards) match
    case Seq(5) => 6 // 5 of a kind
    case Seq(4, _*) => 5 // four of a kind, 1 spare
    case Seq(3, 2) => 4 // full house (3 of a kind, 2 of a kind)
    case Seq(3, _*) => 3 // three of a kind, 2 spare
    case Seq(2, 2, _*) => 2 // two pair, 1 spare
    case Seq(2, _*) => 1 // one pair, 3 spare
    case _ => 0 // high card

  (x: Hand, y: Hand) =>
    val (scoreX, scoreY) = (score(x.cards), score(y.cards))
    if scoreX == scoreY then
      Ordering[Seq[Card]].compare(x.cards, y.cards)
    else
      scoreX.compare(scoreY)

object BasicOrderings:
  given Ordering[Card] = Ordering.by(0 - _.ordinal)

object WithJokerOrderings:
  def rank(c: Card) = if c == Card.J then Int.MinValue else 0 - c.ordinal
  given Ordering[Card] = Ordering.by(rank)

def solution(input: String, grouping: HandGrouping)(using Ordering[Card]): Long =
  given Ordering[Hand] = orderHands(grouping)
  parse(input).sorted.zipWithIndex
    .map: (hand, score) =>
      hand.bid * (score + 1).toLong
    .sum

def part1(input: String): Long =
  import BasicOrderings.given
  solution(input, groupBasic)

def part2(input: String): Long =
  import WithJokerOrderings.given
  solution(input, groupWithJoker)


import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

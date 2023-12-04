package day04

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*
import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

type Card = (id: Int, wins: Int)

def parse(card: String): Card =
  val (r"Card ${Seq(r"$id%d")}..!(\\s+): ${r"$winning%d"}..!(\\s+) | ${r"$mine%d"}..!(\\s+)") = card: @unchecked
  (id = id, wins = (winning.toSet `intersect` mine.toSet).size)

def part1(input: String): Int = input.linesIterator.map(parse andThen score).sum.toInt

def score(card: Card): Int =
  math.floor(math.pow(2, card.wins - 1)).toInt

def addCopies(state: Map[Int, Int], card: Card): Map[Int, Int] =
  val currentCopies = state.getOrElse(card.id, 0) + 1
  val state0 = state + (card.id -> currentCopies)
  (card.id to card.id + card.wins).tail.foldLeft(state0): (state0, id) =>
    state0.updatedWith(id):
      case Some(count) => Some(count + currentCopies)
      case None => Some(currentCopies)

def part2(input: String): Int =
  input.linesIterator.map(parse).foldLeft(Map.empty)(addCopies).values.sum

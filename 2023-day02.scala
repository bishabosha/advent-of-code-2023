package day02

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.r

type Colors = (color: String, count: Int)
type Game = (game: Int, hands: Seq[Seq[Colors]])
type Config = Map[String, Int]

def validate(config: Config, game: Game): Boolean =
  game.hands.forall:
    _.forall:
      case (color, count) => config.getOrElse(color, 0) >= count

def parse(line: String): Game =
  val (r"Game $id%d: ${r"${r"$countss%d $namess"}...(, )"}...(; )") = line: @unchecked
  (game = id, hands = namess.lazyZip(countss).map(_.zip(_)))

def part1(input: String): Int =
  val clauses = input.linesIterator.map(parse).toList
  val config = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14,
  )
  clauses.collect({ case game if validate(config, game) => game.game }).sum

def part2(input: String): Int =
  val clauses = input.linesIterator.map(parse).toList
  val initial = Seq("red", "green", "blue").map(_ -> 0).toMap

  def minCubes(game: Game): Int =
    val maximums = game.hands.foldLeft(initial): (maximums, colors) =>
      colors.foldLeft(maximums):
        case (maximums, (color, count)) =>
          maximums + (color -> (maximums(color) `max` count))
    maximums.values.product

  clauses.map(minCubes).sum

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

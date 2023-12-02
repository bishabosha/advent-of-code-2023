package day02

import scala.language.experimental.namedTuples

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

type Colors = (color: String, count: Int)
type Game = (game: Int, groups: List[List[Colors]])
type Config = Map[String, Int]

def validate(config: Config, game: Game): Boolean =
  game.groups.forall: colors =>
    colors.forall(c => config.getOrElse(c.color, 0) >= c.count)

def parseColors(pair: String): Colors =
  val Array(count0, color0) = pair.split(" ")
  (color = color0, count = count0.toInt)

def parse(line: String): Game =
  val Array(game0, groups) = line.split(": "): @unchecked
  val Array(_, id) = game0.split(" "): @unchecked
  val groups0 = groups.split("; ").toList
  val groups1 = groups0.map(_.split(", ").map(parseColors).toList)
  (game = id.toInt, groups = groups1)

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
    val maximums = game.groups.foldLeft(initial): (maximums, colors) =>
      colors.foldLeft(maximums): (maximums, color) =>
        maximums + (color.color -> (maximums(color.color) `max` color.count))
    maximums.values.product

  clauses.map(minCubes).sum

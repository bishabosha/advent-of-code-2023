package day11

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*

type Coord = (x: Int, y: Int)
type World = (galaxies: Seq[Coord], emptyCols: Seq[Int], emptyRows: Seq[Int])

def parse(input: String): World =
  val r"${rows}...(\n)" = input: @unchecked
  val emptyRows = rows.indices.filter(rows(_).forall(_ == '.'))
  val emptyCols =
    rows.head.indices.filter(x => rows.forall(row => row(x) == '.'))
  val galaxies = rows.indices.flatMap: y =>
    rows(y).indices.collect:
      case x if rows(y)(x) == '#' => (x = x, y = y)
  (galaxies, emptyCols, emptyRows)

def expand(world: World, expansionFactor: Int): Seq[Coord] =
  val galaxies = world.galaxies
  val exp = expansionFactor - 1
  val patches = galaxies.map(_ -> (x = 0, y = 0)).toMap
  val expandedYs = world.emptyRows.foldLeft(patches): (acc, y) =>
    val after = galaxies.filter(_.y > y)
    after.foldLeft(acc): (acc, c) =>
      val patch = acc(c)
      acc + (c -> (patch.x, patch.y + exp))
  val expandedXYs = world.emptyCols.foldLeft(expandedYs): (acc, x) =>
    val after = galaxies.filter(_.x > x)
    after.foldLeft(acc): (acc, c) =>
      val patch = acc(c)
      acc + (c -> (patch.x + exp, patch.y))
  val expandedXY: Seq[Coord] = expandedXYs.toSeq.map((c, patch) =>
    (c.x + patch.x, c.y + patch.y)
  )
  expandedXY.toIndexedSeq

def solve(input: String, expansionFactor: Int): Long =
  val galaxies = expand(parse(input), expansionFactor)
  val lengths = galaxies.indices.combinations(2).map:
    case Seq(i, j) =>
      val a = galaxies(i)
      val b = galaxies(j)
      val dx = b.x.toLong - a.x
      val dy = b.y.toLong - a.y
      dx.abs + dy.abs
  lengths.sum

def part1(input: String): Long =
  solve(input, expansionFactor = 2)

def part2(input: String): Long =
  solve(input, expansionFactor = 1_000_000)

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

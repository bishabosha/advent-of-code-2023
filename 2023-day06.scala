package day06

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.r

type Race = (time: Long, distance: Long)

def part1(input: String): Long =
  val r"Time:${r"$times%L"}..!(\\s+)Distance:${r"$distances%L"}..!(\\s+)" = input.trim(): @unchecked
  // x * (t - x) > d
  // (x * t) - x^2 > d
  // x^2 - (t * x) + d < 0
  def valid(race: Race, x: Long) =
    (x * x) - (race.time * x) + race.distance < 0

  val races: Seq[Race] = times.zip(distances)

  val ways = races.map: race =>
    (0L to race.time).count(valid(race, _))

  ways.product
end part1

def part2(input: String): Long =
  val r"Time:${r"$times"}..!(\\s+)Distance:${r"$distances"}..!(\\s+)" = input.trim(): @unchecked

  def mixDigits(digitss: Seq[String]) =
    digitss.foldLeft(0L): (acc, digits) =>
      digits.foldLeft(acc): (acc, digit) =>
        acc * 10 + digit.asDigit

  val race = (time = mixDigits(times), distance = mixDigits(distances))

  // root1 < (t +- sqrt(t^2 - 4d)) / 2 < root2
  def solve(race: Race) =
    val top = math.sqrt(race.time * race.time - 4 * race.distance)
    val root1 = race.time - top / 2
    val root2 = race.time + top / 2
    // credit to github.com/spamegg1 for these insights:
    // 1) we need the integers strictly between the roots,
    //    so round up the smaller root, round down the larger root.
    val (r1, r2) = (root1.ceil.toLong, root2.floor.toLong)
    // 2) Additionally, if the root is itself an integer then
    //    that would cause an exact solution, which we don't want because
    //    that would equal the winning time, not beat it.
    //    therefore we need to round up further any roots that are integers.
    val lower = if r1 == root1 then r1 + 1L else r1
    val upper = if r2 == root2 then r2 - 1L else r2
    (lower, upper)

  val (lower, upper) = solve(race)
  upper - lower + 1
end part2


import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

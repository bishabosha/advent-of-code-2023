package day18

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*

// follow the shoelace formula (Trapezoid form) for O(n) solution
// https://en.wikipedia.org/wiki/Shoelace_formula

type Command = (dir: Direction, steps: Long)

import scala.util.FromDigits

enum Direction:
  case R, D, L, U

  def translate(x: Long, y: Long, steps: Long): (x: Long, y: Long) =
    this match
      case Direction.R => (x = x + steps, y = y)
      case Direction.D => (x = x,         y = y + steps)
      case Direction.L => (x = x - steps, y = y)
      case Direction.U => (x = x,         y = y - steps)

def parse1(input: String): IndexedSeq[Command] =
  val (r"${r"$dirs $stepss%L $_"}...(\n)") = input: @unchecked
  dirs.lazyZip(stepss).map: (dir, steps) =>
    (Direction.valueOf(dir), steps)

def parse2(input: String): IndexedSeq[Command] =
  val (r"${r"$_ (#$hexs)"}...(\n)") = input: @unchecked
  hexs.map: h =>
    (Direction.fromOrdinal(h.last.asDigit), FromDigits.longFromDigits(h.init, radix = 16))

def solve(commands: Seq[Command]): Long =
  val initial = (x = 0L, y = 0L)
  val (area, _) = commands.foldLeft((area = 1L, v = initial)): (state, command) =>
    val (dir, steps) = command
    val (area, (x, y)) = state
    val (next @ (x1, _)) = dir.translate(x, y, steps)
    val area1 = dir match
      case Direction.R => area + steps
      case Direction.D => area + (steps * (x1 + 1)) // include area of current edge
      case Direction.L => area // botton edge is already included by D
      case Direction.U => area - (steps * x1) // remove area to left of current edge
    (area1, next)
  area

def part1(input: String): Long =
  solve(parse1(input))

def part2(input: String): Long =
  solve(parse2(input))

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

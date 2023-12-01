package day01

import scala.language.experimental.namedTuples

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

type Digits = (tens: Int, units: Int)

extension (ds: Digits) def toInt = ds.tens * 10 + ds.units

def basicMatch(line: String) = (
  tens  = line.iterator.filter(_.isDigit).next().asDigit,
  units = line.reverseIterator.filter(_.isDigit).next().asDigit
)

def solution(input: String, search: String => Digits): Int =
  input.linesIterator.map(search(_).toInt).sum

def part1(input: String): Int = solution(input, basicMatch)

val fullDigits =
  IArray(
    "one", "two", "three",
    "four", "five", "six",
    "seven", "eight", "nine"
  ) ++ (1 to 9).map(_.toString)

def fullMatch(line: String) =
  (
    tens  = line.tails.map(t => fullDigits.indexWhere(t.startsWith)).filter(_ >= 0).next() % 9 + 1,
    units = line.inits.map(t => fullDigits.indexWhere(t.endsWith)).filter(_ >= 0).next() % 9 + 1
  )

def part2(input: String): Int = solution(input, fullMatch)

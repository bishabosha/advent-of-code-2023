package day09

import regexglob.RegexGlobbing.r

type Num = Long
type Nums = Seq[Num]

def parse(line: String): Nums =
  val r"${r"$is%L"}...( )" = line: @unchecked
  is

def diffs(is: Nums): Nums =
  is.sliding(2).map { case Seq(a, b) => b - a }.toSeq

def nextValue(ns: Nums, pick: Nums => Num, combine: (Num, Num) => Num): Num =
  val sub = diffs(ns)
  if sub.forall(_ == 0L) then
    pick(ns)
  else
    combine(pick(ns), nextValue(sub, pick, combine))

def solution(input: String, next: Nums => Num): Num =
  input.linesIterator.map(parse andThen next).sum

def part1(input: String): Num = solution(input, nextValue(_, _.last, _ + _))

def part2(input: String): Num = solution(input, nextValue(_, _.head, _ - _))

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

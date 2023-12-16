package day12

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*

type Record = (springs: String, groups: Seq[Int])

def parse(input: String): Seq[Record] =
  val r"${r"$groups ${r"$iss%d"}...(,)"}...(\n)" = input: @unchecked
  groups.zip(iss)

enum Sub:
  case None, Rock, Space

type Key = (springs: String, idx: Int, sub: Sub, groups: Seq[Int], begin: Int)

val cache = collection.mutable.Map.empty[Key, Long]

def loop(springs: String, idx: Int, sub: Sub, groups: Seq[Int], begin: Int): Long =
  def inGroup = begin >= 0L
  def matchGroup = inGroup && groups.headOption.exists(_ == idx - begin)
  def continue = loop(springs, idx + 1, Sub.None, groups, begin)
  def beginGroup =
    if groups.isEmpty then 0L // does not match
    else loop(springs, idx + 1, Sub.None, groups, begin = idx) // continue in next group
  def commitGroup =
    if matchGroup then loop(springs, idx + 1, Sub.None, groups.tail, begin = -1) // consume group
    else 0L // does not match
  def unexpectedFinalGroups =
    groups.sizeIs > (if inGroup then 1 else 0)
  def inner =
    if idx == springs.size then
      if unexpectedFinalGroups || inGroup && !matchGroup then 0L // did not match
      else 1L // all done
    else (springs(idx), sub) match
      case ('#', _) | ('?', Sub.Rock) =>
        if inGroup then continue else beginGroup
      case ('.', _) | ('?', Sub.Space) =>
        if inGroup then commitGroup else continue
      case ('?', Sub.None) =>
        loop(springs, idx, Sub.Space, groups, begin) + loop(springs, idx, Sub.Rock, groups, begin)
      case _ => 0L // does not match

  cache.getOrElseUpdate((springs, idx, sub, groups, begin), inner)
end loop

def arrangements(record: Record): Long =
  try loop(record.springs, 0, Sub.None, record.groups, begin = -1)
  finally cache.clear()

def arrangements2(record: Record): Long =
  val augmented = record.springs + (s"?${record.springs}" * 4)
  val groups2 = Seq.fill(5)(record.groups).flatten
  arrangements((augmented, groups2))

def part1(input: String): Long =
  parse(input).map(arrangements).sum

def part2(input: String): Long =
  parse(input).map(arrangements2).sum


import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

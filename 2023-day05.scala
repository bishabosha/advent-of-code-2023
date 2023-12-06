package day05

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.r

type Range = (dest: Long, src: Long, len: Long)

def parse(input: String) =
  val r"seeds: ${r"$seeds%L"}...( )\n\n$rows...(\n\n)" = input: @unchecked
  val r"$ids map:\n${r"$destss%L $srcss%L $lenss%L"}...(\n)" = rows: @unchecked
  val rangess = destss.lazyZip(srcss).lazyZip(lenss).map((dests, srcs, lens) =>
    dests.lazyZip(srcs).lazyZip(lens).map((dest, src, len) =>
      (dest = dest, src = src, len = len)
    ).sortBy(_.src)
  )
  (seeds = seeds, rangess = rangess)

def part1(input: String): Long =
  val (seeds, rangess) = parse(input)
  val locations = rangess.foldLeft(seeds): (seeds, ranges) =>
    seeds.map: seed =>
      ranges.find(range => range.src <= seed && seed < range.src + range.len) match
        case Some(range) => range.dest + (seed - range.src)
        case None => seed
  locations.min

type Resource = (start: Long, len: Long)
type RangeMap = (dest: Long, src: Long, len: Long)
type Division = (committed: List[Resource], explore: Option[Resource])

def divide(seedRange: Resource, range: RangeMap): Division =
  val seedEnd = seedRange.start + seedRange.len
  val rangeEnd = range.src + range.len
  val pre = Option.when(seedRange.start < range.src):
    val last = seedEnd min range.src
    (start = seedRange.start, len = last - seedRange.start)
  val overlap = Option.when(seedRange.start < rangeEnd && seedEnd > range.src):
    val begin = seedRange.start max range.src
    val last = seedEnd min rangeEnd
    val offset = range.dest - range.src
    (start = begin + offset, len = last - begin)
  val post = Option.when(seedEnd > rangeEnd):
    val begin = seedRange.start max rangeEnd
    (start = begin, len = seedEnd - begin)
  (pre.toList ++ overlap, post)


def part2(input: String): Long =
  val (seeds, rangess) = parse(input)
  val seedRanges = seeds.grouped(2).map({ case Seq(head, tail) =>
    (start = head, len = tail)
  }).toList
  val locations = rangess.foldLeft(seedRanges): (resources, ranges) =>
    resources.flatMap: resource =>
      val initial: Division = (committed = Nil, explore = Some(resource))
      val next = ranges.foldLeft(initial): (state, range) =>
        state.explore match
          case Some(seedRange) =>
            val next = divide(seedRange, range)
            (state.committed ++ next.committed, next.explore)
          case None => state // skip this range
      next.committed ++ next.explore
  locations.map(_.start).minOption.getOrElse(-1L)

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

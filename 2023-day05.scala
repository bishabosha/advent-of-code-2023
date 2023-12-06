package day05

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.r

type Resource = (start: Long, len: Long)
type Lookup = (dest: Long, src: Long, len: Long)
type RangeMap = (lookups: Seq[Lookup])
type Division = (committed: List[Resource], explore: Option[Resource])

def parseSingle(input: String): (seeds: Seq[Long], maps: Seq[RangeMap]) =
  val r"seeds: ${r"$seeds%L"}...( )\n\n$rows...(\n\n)" = input: @unchecked
  val r"$_ map:\n${r"$destss%L $srcss%L $lenss%L"}...(\n)" = rows: @unchecked
  val maps = destss.lazyZip(srcss).lazyZip(lenss).map: (dests, srcs, lens) =>
    (lookups = dests.lazyZip(srcs).lazyZip(lens)
      .map: (dest, src, len) =>
        (dest = dest, src = src, len = len)
      .sortBy(_.src)
    )
  (seeds = seeds, maps = maps)

def parseRanges(input: String): (resources: Seq[Resource], maps: Seq[RangeMap]) =
  val parsed = parseSingle(input)
  val seedRanges = parsed.seeds.grouped(2).map:
    case Seq(head, tail) => (start = head, len = tail)
  (resources = seedRanges.toList, maps = parsed.maps)

def divide(seedRange: Resource, lookup: Lookup): Division =
  val seedEnd = seedRange.start + seedRange.len
  val lookupEnd = lookup.src + lookup.len
  val pre = Option.when(seedRange.start < lookup.src):
    val last = seedEnd min lookup.src
    (start = seedRange.start, len = last - seedRange.start)
  val overlap = Option.when(seedRange.start < lookupEnd && seedEnd > lookup.src):
    val begin = seedRange.start max lookup.src
    val last = seedEnd min lookupEnd
    val offset = lookup.dest - lookup.src
    (start = begin + offset, len = last - begin)
  val post = Option.when(seedEnd > lookupEnd):
    val begin = seedRange.start max lookupEnd
    (start = begin, len = seedEnd - begin)
  (pre.toList ++ overlap, post)

def mapRange(map: RangeMap)(resource: Resource): Seq[Resource] =
  val initial: Division =
    (committed = Nil, explore = Some(resource))
  val next = map.lookups.foldLeft(initial): (acc, lookup) =>
    acc.explore match
      case Some(seedRange) => // still have some seeds unmapped
        val next = divide(seedRange, lookup)
        (acc.committed ++ next.committed, next.explore)
      case None => acc // all seeds mapped, nothing to do
  next.committed ++ next.explore // append any remaining resources

def mapSingle(map: RangeMap)(resource: Long): Long =
  def overlaps(lookup: Lookup) =
    lookup.src <= resource && resource < lookup.src + lookup.len
  map.lookups.find(overlaps) match
    case Some(lookup) => lookup.dest + (resource - lookup.src)
    case None => resource

def part1(input: String): Long =
  val (resources0, maps) = parseSingle(input)
  val locations = maps.foldLeft(resources0): (resourcesN, map) =>
    resourcesN.map(mapSingle(map))
  locations.min

def part2(input: String): Long =
  val (resources0, maps) = parseRanges(input)
  val locations = maps.foldLeft(resources0): (resourcesN, map) =>
    resourcesN.flatMap(mapRange(map))
  locations.map(_.start).min

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

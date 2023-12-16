package day14

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*
import scala.util.boundary, boundary.break
import scala.util.chaining.*

type Elem = 'O' | '#' | '.'
type World = IndexedSeq[IndexedSeq[Elem]]

def parse(input: String): World =
  val r"$rows...(\n)" = input: @unchecked
  rows.map(_.map[Elem] { case e: Elem => e })

def tiltRow(row: IndexedSeq[Elem]): IndexedSeq[Elem] =
  val (rocks, rest) =
    val g = row.zipWithIndex.groupBy((e, _) => e == '#').withDefault(_ => IndexedSeq.empty)
    (g(true), g(false))
  val (lasts, groupedRocks0) = rocks.foldLeft((lasts = IndexedSeq.empty[(Elem, Int)], groups = IndexedSeq.empty[IndexedSeq[(Elem, Int)]])) { (acc, e) =>
    val (lasts, groups) = acc: @unchecked
    lasts match
      case _ :+ last =>
        if e(1) == last(1) + 1 then
          (lasts :+ e, groups)
        else
          (Vector(e), groups :+ lasts)
      case _ =>
        (Vector(e), groups)
  }
  val groupedRocks = if lasts.nonEmpty then groupedRocks0 :+ lasts else groupedRocks0
  val (explore, groupedRest) = groupedRocks.map(_(0)).foldLeft((explore = rest, groups = IndexedSeq.empty[IndexedSeq[(Elem, Int)]])) { (acc, e) =>
    val (explore, rest) = acc
    val (group, explore1) = explore.span(_(1) < e(1))
    (explore1, rest :+ group)
  }
  val groups = if explore.nonEmpty then (groupedRest :+ explore) else groupedRest
  val sorted = groups.map(_.sortBy(_(0) == '.')) // put 'O' first, then '.''
  val interspersed: IndexedSeq[(Elem, Int)] =
    if groupedRocks.isEmpty then
      sorted.flatten
    else
      val bound = groupedRocks.size max sorted.size
      val rocks0 = groupedRocks.padTo(bound, IndexedSeq.empty)
      val groups0 = groups.padTo(bound, IndexedSeq.empty)
      val sorted0 = sorted.padTo(bound, IndexedSeq.empty)
      groups0.lazyZip(sorted0).lazyZip(rocks0).flatMap { (group, sorted, rocks) =>
        (group.headOption, rocks.headOption) match
          case (Some(e), Some(r)) =>
            if e(1) < r(1) then
              sorted ++ rocks
            else
              rocks ++ sorted
          case (Some(e), _) =>
            sorted
          case (_, Some(r)) =>
            rocks
          case _ =>
            IndexedSeq.empty
      }
  interspersed.map[Elem](_(0))

def tiltNorth(world: World): World =
  val cols = world.transpose[Elem]
  cols.map(tiltRow).transpose

def tiltSouth(world: World): World =
  val cols = world.transpose[Elem]
  cols.map(((_: IndexedSeq[Elem]).reverse) andThen tiltRow andThen (_.reverse)).transpose

def tiltEast(world: World): World =
  world.map(((_: IndexedSeq[Elem]).reverse) andThen tiltRow andThen (_.reverse))

def tiltWest(world: World): World =
  world.map(tiltRow)

def tiltCycle(world: World): World =
  tiltEast(tiltSouth(tiltWest(tiltNorth(world))))

def totalLoad(world: World): Int =
  val length = world.length
  world.zipWithIndex
    .map: (row, y) =>
      row.collect({ case 'O' => length - y }).sum
    .sum

def part1(input: String): Int =
  val world = parse(input)
  val tilted = tiltNorth(world)
  // println(s"initial:\n${world.map(_.mkString).mkString("\n")}\ntilted:\n${tilted.map(_.mkString).mkString("\n")}")
  totalLoad(tilted)

def part2(input: String): Int = boundary:
  val world = parse(input)
  val turns = scala.collection.mutable.ArrayBuffer.empty[World]
  val lookup = scala.collection.mutable.HashMap.empty[World, Int]
  (0 until 1_000_000_000).foldLeft(world)((w, i) =>
    lookup.get(w) match
      case Some(from) =>
        val remaining = (1_000_000_000 - i - 1) % (i - from)
        break(totalLoad(turns(from + remaining))) // shortcut
      case None =>
        tiltCycle(w).tap: res =>
          lookup(w) = i
          turns += res
  )
  -1 // never found a cycle
end part2

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
